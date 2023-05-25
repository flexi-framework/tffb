# =========================================================================
# Get TensorFlow
# =========================================================================
MESSAGE(STATUS "Using TensorFlow and compiling FLEXI-TF-Binding")
IF (HOMERUN GREATER -1)
  SET (TF_DOWNLOAD ${LIBS_DLPATH}libs/tensorflow-2.4.git)
ELSE()
  SET (TF_DOWNLOAD ${LIBS_DLPATH}libs/tensorflow-2.4.git)
ENDIF()
SET(TF_DIR ${LIBS_EXTERNAL_LIB_DIR}/tensorflow)
# Try to find the library in the share directory
FIND_PATH(   TF_INCLUDES c_api.h           HINTS ${TF_DIR}/include ${TF_DIR}/include/tensorflow PATH_SUFFIXES c core)
FIND_LIBRARY(TF_LIBRARIES NAMES tensorflow HINTS ${TF_DIR}/lib)
IF(TF_INCLUDES STREQUAL "TF_INCLUDES-NOTFOUND" OR TF_LIBRARIES STREQUAL "TF_LIBRARIES-NOTFOUND")
  # If not found, clone prebuilt TF from IAG-Gitlab repository and 'skip' building process
  MESSAGE(STATUS "TensorFlow not found, fetching prebuilt library from GitLab")
  ExternalProject_Add(tensorflow_fetch
    PREFIX ${TF_DIR}
    GIT_REPOSITORY ${TF_DOWNLOAD}
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND ""
  )
  # Then extract the prebuilt library
  ADD_CUSTOM_TARGET(tensorflow tar -xzf ${TF_DIR}/src/tensorflow_fetch/*.tar.gz -C ${TF_DIR})
  # First download then extract
  ADD_DEPENDENCIES(tensorflow tensorflow_fetch)
ELSE()
  MESSAGE(STATUS "TensorFlow found: " ${TF_LIBRARIES})
  ADD_CUSTOM_TARGET(tensorflow) # dummy target
ENDIF()
INCLUDE_DIRECTORIES(   ${TF_DIR}/include)
LINK_DIRECTORIES(      ${TF_DIR}/lib)
LIST(APPEND linkedlibs ${TF_DIR}/lib/libtensorflow_framework.so)
LIST(APPEND linkedlibs ${TF_DIR}/lib/libtensorflow.so)

# =========================================================================
# Build FLEXI-TF-Interface
# =========================================================================
SET(LIB_NAME "tffb")

FILE(GLOB ${LIB_NAME}F ./${LIB_NAME}/*.f90
                       ./${LIB_NAME}/*.cpp)
ADD_LIBRARY(${LIB_NAME} STATIC ${LIB_NAME}F)
SET_TARGET_PROPERTIES(${LIB_NAME} PROPERTIES LINKER_LANGUAGE C
                                             COMPILE_FLAGS "-fPIC")
ADD_DEPENDENCIES(${LIB_NAME} tensorflow)
LIST(APPEND INTERNALLIBS ${LIB_NAME})
LIST(APPEND linkedlibs   ${LIB_NAME})
