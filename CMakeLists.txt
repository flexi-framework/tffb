#=================================================================================================================================
# Licensed under the MIT License <http://opensource.org/licenses/MIT>.
# SPDX-License-Identifier: MIT
# Copyright (c) 2023 Prof. Andrea Beck
#
# Permission is hereby  granted, free of charge, to any  person obtaining a copy
# of this software and associated  documentation files (the "Software"), to deal
# in the Software  without restriction, including without  limitation the rights
# to  use, copy,  modify, merge,  publish, distribute,  sublicense, and/or  sell
# copies  of  the Software,  and  to  permit persons  to  whom  the Software  is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE  IS PROVIDED "AS  IS", WITHOUT WARRANTY  OF ANY KIND,  EXPRESS OR
# IMPLIED,  INCLUDING BUT  NOT  LIMITED TO  THE  WARRANTIES OF  MERCHANTABILITY,
# FITNESS FOR  A PARTICULAR PURPOSE AND  NONINFRINGEMENT. IN NO EVENT  SHALL THE
# AUTHORS  OR COPYRIGHT  HOLDERS  BE  LIABLE FOR  ANY  CLAIM,  DAMAGES OR  OTHER
# LIABILITY, WHETHER IN AN ACTION OF  CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE  OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#=================================================================================================================================

CMAKE_MINIMUM_REQUIRED(VERSION 3.0.0)

PROJECT(TFFB C CXX Fortran)

# =========================================================================
# Initialization
# =========================================================================

# Set output paths
SET(LIBRARY_OUTPUT_PATH lib)
SET(EXECUTABLE_OUTPUT_PATH bin)
MARK_AS_ADVANCED(LIBRARY_OUTPUT_PATH EXECUTABLE_OUTPUT_PATH)

# Additional modules
INCLUDE(ExternalProject)
INCLUDE(CTest)

# Build Types
IF (NOT CMAKE_BUILD_TYPE)
  SET(CMAKE_BUILD_TYPE "Release" CACHE STRING "Build Type: 'Release' or 'Debug'" FORCE)
ENDIF()
SET_PROPERTY(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Release" "Debug")

# =========================================================================
# Get TensorFlow
# =========================================================================
MESSAGE(STATUS "Checking for TensorFlow")

SET(TF_GPU "cpu" CACHE STRING "Use TensorFlow with or without GPU support")
SET_PROPERTY(CACHE TF_GPU PROPERTY STRINGS "cpu" "gpu")

SET(TF_VERSION "2.9.1" CACHE STRING "Version of TensorFlow that should be installed")
SET(TF_DOWNLOAD https://storage.googleapis.com/tensorflow/libtensorflow/libtensorflow-${TF_GPU}-linux-x86_64-${TF_VERSION}.tar.gz
)
SET(TF_DIR ${CMAKE_SOURCE_DIR}/share/tensorflow-${TF_GPU}-${TF_VERSION})
# Try to find the library in the share directory
FIND_PATH(   TF_INCLUDES tensorflow/c/c_api.h            HINTS    ${TF_DIR}/include/ $ENV{TF_DIR}/include/)
FIND_LIBRARY(TF_LIB           NAMES tensorflow           HINTS    ${TF_DIR}/lib/     $ENV{TF_DIR}/lib/    )
FIND_LIBRARY(TF_LIB_FRAMEWORK NAMES tensorflow_framework HINTS    ${TF_DIR}/lib/     $ENV{TF_DIR}/lib/    )
IF(TF_INCLUDES      STREQUAL "TF_INCLUDES-NOTFOUND"    OR
   TF_LIB           STREQUAL "TF_LIB-NOTFOUND"         OR
   TF_LIB_FRAMEWORK STREQUAL "TF_LIB_FRAMEWORK-NOTFOUND")
  # If not found, clone prebuilt TF and 'skip' building process
  MESSAGE(STATUS "TensorFlow not found, fetching prebuilt library!")
  ExternalProject_Add(tensorflow
    DOWNLOAD_DIR ${TF_DIR}
    SOURCE_DIR ${TF_DIR}
    URL ${TF_DOWNLOAD}
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND ""
  )
  INCLUDE_DIRECTORIES(${TF_DIR}/include)
  LIST(APPEND TF_LIBS ${TF_DIR}/lib/libtensorflow.so ${TF_DIR}/lib/libtensorflow_framework.so)
ELSE()
  MESSAGE(STATUS "TensorFlow found: " ${TF_LIB})
  ADD_CUSTOM_TARGET(tensorflow)
  INCLUDE_DIRECTORIES(${TF_INCLUDES}/ )
  LIST(APPEND TF_LIBS ${TF_LIB} ${TF_LIB_FRAMEWORK})
ENDIF()

# =========================================================================
# Build TensorFlow-Fortran-Binding (TFFB)
# =========================================================================
MESSAGE(STATUS "Building TFFB libraries")

SET(TFFB_INCLUDE_DIR ${CMAKE_BINARY_DIR}/include)
FILE(GLOB TFFB_FILES ./src/*.f90
                     ./src/*.cpp)
ADD_LIBRARY(tffb SHARED ${TFFB_FILES})
ADD_DEPENDENCIES(tffb tensorflow)
SET_TARGET_PROPERTIES(tffb PROPERTIES LINKER_LANGUAGE C
                                      COMPILE_FLAGS "-fPIC"
                                      Fortran_MODULE_DIRECTORY ${TFFB_INCLUDE_DIR})
TARGET_LINK_LIBRARIES(tffb ${TF_LIBS})

# Static library
#ADD_LIBRARY(tffbstatic STATIC ${TFFB_FILES})
#ADD_DEPENDENCIES(tffbstatic tensorflow tffb) # Avoid race conditions
#SET_TARGET_PROPERTIES(tffbstatic PROPERTIES LINKER_LANGUAGE C
#                                            COMPILE_FLAGS "-fPIC"
#                                            Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)

# =========================================================================
# Build Test
# =========================================================================
MESSAGE(STATUS "Building Tests")

ADD_EXECUTABLE(test_run ./tests/test_run.f90)
TARGET_LINK_LIBRARIES(test_run tffb ${TF_LIBS})
INCLUDE_DIRECTORIES(${TFFB_INCLUDE_DIR})
ADD_DEPENDENCIES(test_run tensorflow tffb) # Avoid race conditions
SET_TARGET_PROPERTIES(test_run PROPERTIES LINKER_LANGUAGE Fortran)
ADD_TEST(NAME test_run COMMAND test_run ${CMAKE_SOURCE_DIR}/tests/model)

# =========================================================================
# Run Tests
# =========================================================================
ADD_CUSTOM_TARGET( all_tests ALL DEPENDS test_run)
ADD_CUSTOM_COMMAND(TARGET all_tests
                   COMMENT "Run unit tests"
                   POST_BUILD COMMAND ctest --output-on-failure
                   WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
