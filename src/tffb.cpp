// Licensed under the MIT License <http://opensource.org/licenses/MIT>.
// SPDX-License-Identifier: MIT
// Copyright (c) 2018 - 2020 Daniil Goncharov <neargye@gmail.com>.
// Copyright (c) [2020-Present] [Romit Maulik and other contributors]
// Copyright (c) 2023 Prof. Andrea Beck
//
// Permission is hereby  granted, free of charge, to any  person obtaining a copy
// of this software and associated  documentation files (the "Software"), to deal
// in the Software  without restriction, including without  limitation the rights
// to  use, copy,  modify, merge,  publish, distribute,  sublicense, and/or  sell
// copies  of  the Software,  and  to  permit persons  to  whom  the Software  is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE  IS PROVIDED "AS  IS", WITHOUT WARRANTY  OF ANY KIND,  EXPRESS OR
// IMPLIED,  INCLUDING BUT  NOT  LIMITED TO  THE  WARRANTIES OF  MERCHANTABILITY,
// FITNESS FOR  A PARTICULAR PURPOSE AND  NONINFRINGEMENT. IN NO EVENT  SHALL THE
// AUTHORS  OR COPYRIGHT  HOLDERS  BE  LIABLE FOR  ANY  CLAIM,  DAMAGES OR  OTHER
// LIABILITY, WHETHER IN AN ACTION OF  CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE  OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

// This work is based on previous works of
// - Daniil Goncharov (https://github.com/Neargye/hello_tf_c_api, MIT license)
// - Romit Maulik and colleagues under
//   * "Deploying deep learning in OpenFOAM with TensorFlow", AIAA Scitech 2021, 2021, Maulik et al.
//   * https://arxiv.org/pdf/2012.00900.pdf
//   * https://github.com/argonne-lcf/TensorFlowFoam, MIT license.


#include <stdio.h>
#include <cstring>
#include <cstdint>
#include <array>
// TensorFlow C API header
#include <tensorflow/c/c_api.h>
#include <tensorflow/c/c_api_experimental.h>

// fuctions have to be 'extern "C"' in order to be called from Fortran
extern "C" void c_tffb_loadmodel(char* modelpath, char* inputnode, char* outputnode, int dowrite, int nthreads);
extern "C" void c_tffb_evalmodel(double *input, size_t n_input, int* shape_input, size_t dim_input, double* output, size_t n_output);
extern "C" void c_tffb_finalizemodel();

// Global variables
TF_Output   input_ph_;
TF_Output   output_;
TF_Graph*   graph_;
TF_Status*  status_;
TF_Session* session_;
TF_SessionOptions* sessionOpts_;


// Create and return tensor populated with given data
TF_Tensor* CreateTensor(TF_DataType data_type,
                        const std::int64_t* dims, std::size_t num_dims,
                        const void* data , std::size_t len) {
  if (dims == nullptr){
    return nullptr;
  }
  TF_Tensor* tensor = TF_AllocateTensor(data_type, dims ,static_cast<int>(num_dims), len);
  if (tensor == nullptr){
    return nullptr ;
  }
  void* tensor_data = TF_TensorData(tensor);
  if (tensor_data == nullptr){
    TF_DeleteTensor(tensor);
    return nullptr;
  }
  if (data != nullptr){
    std::memcpy(tensor_data, data, std::min(len, TF_TensorByteSize(tensor)));
  }
  return tensor;
}


// Delete TF Tensor if allocated
void DeleteTensor(TF_Tensor* tensor){
  if (tensor != nullptr){
    TF_DeleteTensor(tensor);
  }
}


// Delete TF session and corresponding data if allocated
void DeleteSession(TF_Session* session){
  TF_Status* status = TF_NewStatus();
  TF_CloseSession(session, status);
  if(TF_GetCode(status) != TF_OK) {
    TF_CloseSession(session, status);
  }
  TF_DeleteSession (session , status);
  if(TF_GetCode (status) != TF_OK) {
    TF_DeleteSession(session, status);
  }
  TF_DeleteStatus (status);
}


// Load TF model from given path and find input and output nodes for inference
void c_tffb_loadmodel(char* modelpath, char* inputnode, char* outputnode, int dowrite, int nthreads){
  graph_ = TF_NewGraph();
  status_ = TF_NewStatus();
  sessionOpts_ = TF_NewSessionOptions();

  // Execute TF on given number of threads per calling rank
  // See https://github.com/tensorflow/tensorflow/issues/13853
  if (nthreads>0){
    uint8_t intra_op_parallelism_threads = nthreads;
    uint8_t inter_op_parallelism_threads = nthreads;
    uint8_t buf[] = {0x10,intra_op_parallelism_threads, 0x28, inter_op_parallelism_threads};
    TF_SetConfig(sessionOpts_, buf, sizeof(buf), status_);
    if(dowrite) printf(" | TensorFlow will start %i threads per MPI rank!\n",nthreads);
  }

  // Enabling XLA compiler increases performance significantly, but might be prone to issues
  TF_EnableXLACompilation(sessionOpts_,true);

  // Load session
  const char* tags[] = {"serve"};
  session_ = TF_LoadSessionFromSavedModel(sessionOpts_, nullptr, modelpath, tags, 1, graph_, nullptr, status_);
  TF_DeleteSessionOptions(sessionOpts_);

  // Safety check
  if(TF_GetCode(status_) == TF_OK){
    if(dowrite) printf(" | Model was successfully loaded from '%s'!\n", modelpath);
  }else{
    printf(" | %s",TF_Message(status_));
  }

  // Input operation
  input_ph_ = {TF_GraphOperationByName(graph_, inputnode), 0};
  if(input_ph_.oper == nullptr){
    printf(" ERROR: Did not find input node '%s'!\n", inputnode);
  }else{
    if(dowrite) printf(" | Input node '%s' found!\n", inputnode);
  }

  // Output operation
  output_ = {TF_GraphOperationByName(graph_, outputnode), 0};
  if(output_.oper == nullptr){
    printf(" ERROR: Did not find output node '%s'!\n", outputnode);
  }else{
    if(dowrite) printf(" | Output node '%s' found!\n", outputnode);
  }
}


// Evaluate loaded TF model. ATTENTION: input data is assumed to be double and TF model expects single prec. float
void c_tffb_evalmodel(double *input, size_t n_input, int *dims, size_t ndims, double *output, size_t n_output) {
  // Cast shape_input to int64_t
  int64_t dims_int64[ndims];
  for (int i=0;i<ndims;i++){
    dims_int64[i] = (int64_t)dims[i];
  }

  // Cast input from double to float
  float input_float32[n_input];
  for (int i=0;i<n_input;i++){
    input_float32[i] = (float)input[i];
  }

  TF_Tensor* output_tensor_ = nullptr;
  TF_Tensor* input_tensor_ = CreateTensor(TF_FLOAT, dims_int64, ndims, input_float32, n_input*sizeof(float));

  // Arrays of tensors
  TF_Tensor* input_tensors_[1] = {input_tensor_};
  TF_Tensor* output_tensors_[1] = {output_tensor_};
  // Arrays of operations
  TF_Output inputs[1] = {input_ph_};
  TF_Output outputs[1] = {output_};

  TF_SessionRun(session_,
                nullptr, // Run options.
                inputs,  input_tensors_ , 1, // Input tensor ops, input tensor values, number of inputs.
                outputs, output_tensors_ , 1, // Output tensor ops, output tensor values, number of outputs.
                nullptr, 0, // Target operations , number of targets.
                nullptr, // Run metadata.
                status_ // Output status.
                );

  // Delete Tensors
  DeleteTensor(input_tensor_);
  DeleteTensor(output_tensor_);

  // Cast output from TF C API to float*
  float* output_float = static_cast <float*>(TF_TensorData(output_tensors_[0]));

  // Cast prediction from float to double
  for (int i=0;i<n_output;i++){
    output[i] =  (double) output_float[i];
  }
}


// Properly deallocate all resources and free memory
void c_tffb_finalizemodel() {
  TF_DeleteGraph(graph_);
  TF_DeleteStatus(status_);
  DeleteSession(session_);
}
