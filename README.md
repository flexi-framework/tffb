# TensorFlow-Fortran-Binding (TFFB)
<p align="center" width="100%">
    <img width="50%" src="doc/tffb.png"> 
</p>
This project contains the TensorFlow Fortran Binding (TFFB) library, which allows to load and evaluate Keras models directly from Fortran in a straight-forward manner.


## License
The TFFB is licensed under the MIT License <http://opensource.org/licenses/MIT>.
For the full license terms see the included license file [license](LICENSE.MD).
The majority of the TFFB is Copyright (c) 2023, Prof. Andrea Beck, while the `src/tf.cpp` file contains code of otherss, for which the original authors hold the Copyright, i.e. Copyright (c) 2018-2020, Daniil Goncharov and Copyright (c) 2020-Present, Romit Maulik and other contributors.
Please also have a look at their projects under <https://github.com/Neargye/hello_tf_c_api> and <https://github.com/argonne-lcf/TensorFlowFoam>, respectively, and have a look at the original publication by Maulik et al. ["Deploying deep learning in OpenFOAM with TensorFlow", AIAA Scitech 2021.](https://arxiv.org/pdf/2012.00900.pdf)


## Installation
The TFFB is built using the `CMake` build system.
The TensorFlow library can be either linked by setting the environment variable `TF_DIR` to the correct folder or a prebuilt version can be dewnloaded from the TensorFlow website automatically during the build process.
For this, we create a build folder with
```
mkdir -p build && cd build
```
The project can be configured either using `ccmake` with the command
```
ccmake ..
```
to use the graphical interface.
For most cases it is sufficient to simply run the commandline version with
```
cmake ..
```
In both cases the version of the downloaded prebuilt TensorFlow binary can be set by changing the variable `TF_VERSION`, e.g. `TF_VERSION=2.9.1` and the flag `TF_GPU` can be set to `cpu` or `gpu` to specify whether the prebuilt library with or without GPU support should be used.
The TFFB can then be built using
```
make
```
If the automatic integration test runs without error, TFFB has been built successfully.


## API
The TFFB implements three main routines.
See `tests/test_run.f90` for examples on how the routines can be called.
The API of the TFFB is defined as follows:

### `TFFB_LoadModel`
```fortran
SUBROUTINE TFFB_LoadModel(ModelPath,ModelInput,ModelOutput,nThreads,doOutput)
! INPUT/OUTPUT VARIABLES
CHARACTER(LEN=255),INTENT(IN)  :: ModelPath   !> Path to folder containing the TensorFlow model in the *.pb format
CHARACTER(LEN=255),INTENT(IN)  :: ModelInput  !> Name of model's input  node for the 'serve' tag
CHARACTER(LEN=255),INTENT(IN)  :: ModelOutput !> Name of model's output node for the 'serve' tag
INTEGER,OPTIONAL,INTENT(IN)    :: nThreads    !> Number of threads used per rank to evaluate model. Default: 1
LOGICAL,OPTIONAL,INTENT(IN)    :: doOutput    !> Whether the calling rank should write to console.  Default: .TRUE.
```
The first routine loads a Keras model from a path, indentifies its input and output nodes and initializes the library.
The `ModelPath` is the path to the folder containing the model and the names of the input and output nodes of the model can be determined with the `saved_model_cli` provided by TensorFlow.
For this, use the command
```bash
saved_model_cli show --tag_set serve --signature_def serving_default --dir path/to/model/
```
which should return for the example model in `tests/model/` something like
```bash
The given SavedModel SignatureDef contains the following input(s):
  inputs['input_1'] tensor_info:
      dtype: DT_FLOAT
      shape: (-1, 6, 6, 6, 3)
      name: serving_default_input_1:0
The given SavedModel SignatureDef contains the following output(s):
  outputs['flatten'] tensor_info:
      dtype: DT_FLOAT
      shape: (-1, 1)
      name: StatefulPartitionedCall:0
Method name is: tensorflow/serving/predict
```
which specified the `dtype`, `shape` and `name` of the input and output nodes.
Hence, in this case `serving_default_input_1` as input and `StatefulPartitionedCall` as output. 


### `TFFB_EvalModel`
```fortran
SUBROUTINE TFFB_EvalModel(nDimsIn,ShapeIn,DataIn,nDimsOut,ShapeOut,DataOut)
! INPUT/OUTPUT VARIABLES
INTEGER,INTENT(IN)    :: nDimsIn                    !> Number of dimensions in the input array
INTEGER,INTENT(IN)    :: ShapeIn(nDimsIn)           !> Shape of input array
REAL(8),INTENT(IN)    :: DataIn(PRODUCT(ShapeIn))   !> Flattened input array with correct number of elements
INTEGER,INTENT(IN)    :: nDimsOut                   !> Number of dimensions in the output array
INTEGER,INTENT(IN)    :: ShapeOut(nDimsOut)         !> Shape of output array
REAL(8),INTENT(INOUT) :: DataOut(PRODUCT(ShapeOut)) !> Flattened output array with correct number of elements
```
The second routines allows to evaluate the loaded model using a single input and output array.
Please note that the input and output arrays are expected to be of kind double precision, i.e. `REAL(8)`.
The arrays can be of arbitrary shape, but the number of dimensions (`nDimsIn/nDimsOut`), the shape of the arrays (`ShapeIn/ShapeOut`) have to be provided alongside the arrays (`DataIn/DataOut`) themselves.
This information is necessary to ensure that the data is passed correctly to the Keras model and fits to its expected shape of input data.

### `TFFB_FinalizeModel`

```fortran
SUBROUTINE TFFB_FinalizeModel()
! INPUT/OUTPUT VARIABLES
```
This routine does not require any arguments but rather frees allocated memory and tears down the initiated TensorFlow session used for evaluating the model.
This routine is expected to be called before a new model is loaded.
