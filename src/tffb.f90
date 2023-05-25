!=================================================================================================================================
! Licensed under the MIT License <http://opensource.org/licenses/MIT>.
! SPDX-License-Identifier: MIT
! Copyright (c) 2023 Prof. Andrea Beck
!
! Permission is hereby  granted, free of charge, to any  person obtaining a copy
! of this software and associated  documentation files (the "Software"), to deal
! in the Software  without restriction, including without  limitation the rights
! to  use, copy,  modify, merge,  publish, distribute,  sublicense, and/or  sell
! copies  of  the Software,  and  to  permit persons  to  whom  the Software  is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE  IS PROVIDED "AS  IS", WITHOUT WARRANTY  OF ANY KIND,  EXPRESS OR
! IMPLIED,  INCLUDING BUT  NOT  LIMITED TO  THE  WARRANTIES OF  MERCHANTABILITY,
! FITNESS FOR  A PARTICULAR PURPOSE AND  NONINFRINGEMENT. IN NO EVENT  SHALL THE
! AUTHORS  OR COPYRIGHT  HOLDERS  BE  LIABLE FOR  ANY  CLAIM,  DAMAGES OR  OTHER
! LIABILITY, WHETHER IN AN ACTION OF  CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE  OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!=================================================================================================================================


!===================================================================================================================================
!> This module contains Fortran wrappers and interfaces to call TensorFlow's C-API from Fortran via the ISO-C-Binding.
!> The C routines themselves rely heavily on
!>  - https://github.com/Neargye/hello_tf_c_api (MIT license)
!>  - "Deploying deep learning in OpenFOAM with TensorFlow", 2021, Maulik et al. (https://arc.aiaa.org/doi/abs/10.2514/6.2021-1485)
!===================================================================================================================================
MODULE TFFB
! MODULES
IMPLICIT NONE
PRIVATE

!-----------------------------------------------------------------------------------------------------------------------------------
! Module variables
LOGICAL           :: TF_ModelIsLoaded=.FALSE. !> Flag specifying whether a model is currently loaded

!-----------------------------------------------------------------------------------------------------------------------------------
! C bindings
INTERFACE
  SUBROUTINE c_tffb_loadmodel(modelpath,inputnode,outputnode,dowrite,nthreads) BIND(C)
      USE ISO_C_BINDING, ONLY: C_CHAR,C_INT
      CHARACTER(KIND=C_CHAR),INTENT(IN) :: modelpath(*)   !> path to folder containing the TensorFlow model in the *.pb format
      CHARACTER(KIND=C_CHAR),INTENT(IN) :: inputnode(*)   !> name of the input node for inference
      CHARACTER(KIND=C_CHAR),INTENT(IN) :: outputnode(*)  !> name of the output node for inference
      INTEGER(KIND=C_INT),VALUE,INTENT(IN) :: dowrite     !> flag to (de)activate output to console
      INTEGER(KIND=C_INT),VALUE,INTENT(IN) :: nthreads    !> number of threads for parallelism in TF
  END SUBROUTINE c_tffb_loadmodel
END INTERFACE

INTERFACE
  SUBROUTINE c_tffb_evalmodel(input,n_input,dims,ndims,output,n_output) BIND(C)
      USE ISO_C_BINDING, ONLY: C_DOUBLE, C_SIZE_T, C_INT
      REAL(KIND=C_DOUBLE),INTENT(IN)           :: input(*)  !> pointer to input data
      INTEGER(KIND=C_SIZE_T),VALUE,INTENT(IN)  :: n_input   !> number of elements in input array
      INTEGER(KIND=C_INT),INTENT(IN)           :: dims(*)   !> shape of input array
      INTEGER(KIND=C_SIZE_T),VALUE,INTENT(IN)  :: ndims     !> number of dimensions of input array
      REAL(KIND=C_DOUBLE),INTENT(IN)           :: output(*) !> pointer to output array
      INTEGER(KIND=C_SIZE_T),VALUE,INTENT(IN)  :: n_output  !> number of elements in output array
  END SUBROUTINE c_tffb_evalmodel
END INTERFACE

INTERFACE
  SUBROUTINE c_tffb_finalizemodel() BIND(C)
  END SUBROUTINE c_tffb_finalizemodel
END INTERFACE

!-----------------------------------------------------------------------------------------------------------------------------------
! Fortran wrappers for C bindings
INTERFACE TFFB_LoadModel
  MODULE PROCEDURE TFFB_LoadModel
END INTERFACE

!INTERFACE TFFB_EvalModel
!  MODULE PROCEDURE TFFB_EvalModel
!END INTERFACE

INTERFACE TFFB_FinalizeModel
  MODULE PROCEDURE TFFB_FinalizeModel
END INTERFACE

PUBLIC :: TFFB_LoadModel,TFFB_EvalModel,TFFB_FinalizeModel
!===================================================================================================================================


CONTAINS


!===================================================================================================================================
!> Fortran wrapper for loading a TensorFlow based on the modelpath and initialize the evaluation based on the names of the input
!> and output nodes.
!===================================================================================================================================
SUBROUTINE TFFB_LoadModel(ModelPath,ModelInput,ModelOutput,nThreads,doOutput)
!-----------------------------------------------------------------------------------------------------------------------------------
! MODULES
USE ISO_C_BINDING, ONLY: C_NULL_CHAR
IMPLICIT NONE
!-----------------------------------------------------------------------------------------------------------------------------------
! INPUT/OUTPUT VARIABLES
CHARACTER(LEN=255),INTENT(IN)  :: ModelPath   !> Path to folder containing the TensorFlow model in the *.pb format
CHARACTER(LEN=255),INTENT(IN)  :: ModelInput  !> Name of model's input  node for the 'serve' tag
CHARACTER(LEN=255),INTENT(IN)  :: ModelOutput !> Name of model's output node for the 'serve' tag
INTEGER,OPTIONAL,INTENT(IN)    :: nThreads    !> Number of threads used per rank to evaluate model. Default: 1
LOGICAL,OPTIONAL,INTENT(IN)    :: doOutput    !> Whether the calling rank should write to console.  Default: .TRUE.
!-----------------------------------------------------------------------------------------------------------------------------------
! LOCAL VARIABLES
INTEGER*4,PARAMETER :: NUL=0  ! C-style boolean value (false) as INT4
INTEGER*4,PARAMETER :: ONE=1  ! C-style boolean value (true) as INT4
INTEGER             :: nThreads_loc
LOGICAL             :: doOutput_loc
!===================================================================================================================================
! Set optional input arguments if present or use default otherwise
doOutput_loc = .TRUE. ! default value
IF (PRESENT(doOutput)) doOutput_loc = doOutput

nThreads_loc = 1 ! default value
IF (PRESENT(nThreads)) nThreads_loc = nThreads

IF (doOutput_loc) WRITE(*,'(A)') 'LOADING SAVED MODEL FROM "'//TRIM(ModelPath)//'" ...'

! Load model via C-API
CALL c_tffb_loadmodel(TRIM(ModelPath  )//C_NULL_CHAR,&
                      TRIM(ModelInput )//C_NULL_CHAR,&
                      TRIM(ModelOutput)//C_NULL_CHAR,&
                      MERGE(ONE,NUL,doOutput_loc),&
                      nThreads_loc)

TF_ModelIsLoaded = .TRUE.

IF (doOutput_loc) WRITE(*,'(A)')'Done !'
END SUBROUTINE TFFB_LoadMOdel


!===================================================================================================================================
!> Fortran wrapper for evaluating loaded TensorFlow model for a single input and output array using the TF C-API
!===================================================================================================================================
SUBROUTINE TFFB_EvalModel(nDimsIn,ShapeIn,DataIn,nDimsOut,ShapeOut,DataOut)
!-----------------------------------------------------------------------------------------------------------------------------------
! MODULES
IMPLICIT NONE
!-----------------------------------------------------------------------------------------------------------------------------------
! INPUT/OUTPUT VARIABLES
INTEGER,INTENT(IN)    :: nDimsIn                    !> Number of dimensions in the input array
INTEGER,INTENT(IN)    :: ShapeIn(nDimsIn)           !> Shape of input array
REAL(8),INTENT(IN)    :: DataIn(PRODUCT(ShapeIn))   !> Flattened input array with correct number of elements
INTEGER,INTENT(IN)    :: nDimsOut                   !> Number of dimensions in the output array
INTEGER,INTENT(IN)    :: ShapeOut(nDimsOut)         !> Shape of output array
REAL(8),INTENT(INOUT) :: DataOut(PRODUCT(ShapeOut)) !> Flattened output array with correct number of elements
!-----------------------------------------------------------------------------------------------------------------------------------
INTEGER*8 :: I8  ! Correct datatype for C_Binding
INTEGER,ALLOCATABLE   :: ShapeIn_C(:),ShapeIn_F(:)
!===================================================================================================================================
IF (.NOT.TF_ModelIsLoaded) THEN
 ERROR STOP 'TF Model should be evaluated but no model is currently loaded!'
END IF

! First, transform invert shape from Fortran into C layout
ALLOCATE(ShapeIn_C(nDimsIn))
ALLOCATE(ShapeIn_F(nDimsIn))
ShapeIn_F = ShapeIn
DO I8=1,nDimsIn
  ShapeIn_C(I8) = ShapeIn_F(nDimsIn-(I8-1))
END DO

! Evaluate model
CALL c_tffb_evalmodel(DataIn,INT(SIZE(DataIn),KIND(I8)),ShapeIn_C,INT(nDimsIn,KIND(I8)),DataOut,INT(SIZE(DataOut),KIND(I8)))

DEALLOCATE(ShapeIn_C,ShapeIn_F)
END SUBROUTINE TFFB_EvalModel


!===================================================================================================================================
!> Fortran wrapper for finalizing the loaded model and free allocated memory
!===================================================================================================================================
SUBROUTINE TFFB_FinalizeModel()
!-----------------------------------------------------------------------------------------------------------------------------------
! MODULES
IMPLICIT NONE
!-----------------------------------------------------------------------------------------------------------------------------------
! INPUT/OUTPUT VARIABLES
!-----------------------------------------------------------------------------------------------------------------------------------
! LOCAL VARIABLES
!===================================================================================================================================
IF (TF_ModelIsLoaded) CALL c_tffb_finalizemodel()
TF_ModelIsLoaded = .FALSE.
END SUBROUTINE TFFB_FinalizeModel

END MODULE TFFB
