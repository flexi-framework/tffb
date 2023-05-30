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
!> Example Program to test installation of TFFB library
!===================================================================================================================================
PROGRAM Test_Run
!-----------------------------------------------------------------------------------------------------------------------------------
! MODULES
USE TFFB
IMPLICIT NONE
!-----------------------------------------------------------------------------------------------------------------------------------
! LOCAL VARIABLES
CHARACTER(LEN=255)  :: model_path  ! Given via commandline
CHARACTER(LEN=255)  :: input_node  ='serving_default_input_1'
CHARACTER(LEN=255)  :: output_node ='StatefulPartitionedCall'
REAL(8)             :: data_input( 3,6,6,6,2)
REAL(8)             :: data_output(        2)
REAL(8),PARAMETER   :: res=4.5335428E-003
REAL(8),PARAMETER   :: eps=1.E-8 ! single precision
INTEGER             :: iVar,i,j,k,iElem
!===================================================================================================================================
CALL GET_COMMAND_ARGUMENT(1,model_path)

! 1. Load model
CALL TFFB_LoadModel(model_path,input_node,output_node)

! 2. Prepare input data
DO k=1,6
  DO j=1,6
    DO i=1,6
      DO iVar=1,3
        data_input(iVar,i,j,k,:) = 2.000*REAL(iVar,8) &
                                 - 1.000*REAL(i   ,8) &
                                 + 0.500*REAL(j   ,8) &
                                 - 0.250*REAL(k   ,8)
      END DO
    END DO
  END DO
END DO

! 3. Evaluate model
WRITE(*,'(A)',ADVANCE='NO') 'Evaluating model...'
CALL TFFB_EvalModel(5,SHAPE(data_input) ,data_input  &
                   ,1,SHAPE(data_output),data_output )
WRITE(*,'(A)') 'DONE!'

! 4. Verify output
WRITE(*,'(A)',ADVANCE='NO') 'Verifying results:'
IF ( (ABS(data_output(1)-res).GT.eps) .OR. &
     (ABS(data_output(1)-res).GT.eps) ) THEN
  WRITE(*,'(A)') '    FAILED!'
  WRITE(*,*) 'Output data was: ', data_output
  WRITE(*,*) 'but is supposed to be:', res
  STOP 2
ELSE
  WRITE(*,'(A)') '    SUCCESSFUL!'
END IF

! 5. finalize
WRITE(*,'(A)',ADVANCE='NO') 'Finalizing model...'
CALL TFFB_FinalizeModel()
WRITE(*,'(A)') 'DONE!'

END PROGRAM TEST_Run
