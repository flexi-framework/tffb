# TensorFlow-Fortran-Binding (TFFB)

This project contains the TFFB (TensorFlow Fortran Binding) library, which allows to load and evaluate Keras models via TensorFlow's C-API.
The library 

## Installation

### TensorFlow
The only prequesite for the TFFB (besides a Fortran and C++ compiler) is TensorFlow's C-API, which can either be compiled from source, or the precompiled version.
For this, download and extract the precompiled library in the required version via
```
TF_VERSION=2.11.0
wget https://storage.googleapis.com/tensorflow/libtensorflow/libtensorflow-cpu-linux-x86_64-${TF_VERSION}.tar.gz
tar -xzf libtensorflow-cpu-linux-x86_64-${TF_VERSION}.tar.gz
```
Now, set the environment variable `TF_DIR` which is used to find the TensorFlow directory later on and add the necessary libraries to your library path via
```
export TF_DIR=$(ls tensorflow)
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${TF_DIR}/lib
```

### TFFB
The TFFB itself can either be used by including its source files directly into existing projects or by compiling it as a library and linking it.
If the environment variable `TF_DIR` and the library path are set correctly, compiling the library can easily be performed by using the Makefile via
```
git clone https://github/flexi-framework/tffb
cd tffb
make
```
The compilation process runs also an initial testprogram that can be found under `tests/`, which loads an example model, evaluates it for some sample inputs and check the return values to be correct.
The output of the test can be found alongside the necesary libraries in the `build` folder.


## Licensing
