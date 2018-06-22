! ERROR - ERROR REPORTING MODULE
!
! Copyright WBM 2007
! Authors: Ian Teakle

MODULE GEN_ERROR
!!DEC$ STRICT

INCLUDE 'COMPILER_DIRECTIVES.FI'

! MODULE USE STATEMENTS
USE PRECISION

! MODULE DECLARATIONS
IMPLICIT NONE

! MODULE ACCESS STATEMENTS
PRIVATE
PUBLIC :: errstat, Nwarn
PUBLIC :: set_model, err, errchk, warn, errhold, errunhold

! MODULE PARAMETER DECLARATIONS
LOGICAL,SAVE,PROTECTED :: errstat=.FALSE.
LOGICAL,SAVE,PROTECTED :: held=.FALSE.
CHARACTER(LEN=LSTR),SAVE,PROTECTED :: errstr=''
INTEGER(KIND=ISGL),SAVE,PROTECTED :: Nwarn=0

! MODULE VARIABLE DECLARATIONS
CHARACTER(LEN=MSTR),SAVE,PROTECTED  ::  model

! MODULE PROCEDURES
CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE set_model(name)
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
CHARACTER(LEN=*),INTENT(IN) :: name

model = name

END SUBROUTINE set_model
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE err(sub,string)
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
CHARACTER(LEN=*),INTENT(IN) :: sub
CHARACTER(LEN=*),INTENT(IN) :: string

!$OMP CRITICAL(e)
IF (.NOT.errstat) THEN ! CATCH FIRST INSTANCE ONLY
    errstat = .TRUE.                                ! ERROR STATUS FLAG: NO ERROR = FALSE
    errstr = trim(sub)//':'//string                 ! ERROR STRING TO BE RETURNED AS PROGRAM STOPS
END IF
!$OMP END CRITICAL(e)

END SUBROUTINE err
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE errchk(sub)
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
CHARACTER(LEN=*),INTENT(IN) :: sub
! LOCAL VARIABLE
INTEGER(KIND=ISGL),PARAMETER :: logunit = 100
CHARACTER(LEN=30),SAVE :: oldsub = ''
LOGICAL :: openstat
INTEGER(KIND=ISGL) :: ierr

!$OMP CRITICAL(c)
IF (errstat .AND. .NOT. held) THEN
    IF (sub==model) THEN
        WRITE(*,'(a)',IOSTAT=ierr)
        WRITE(*,'(a)',IOSTAT=ierr) 'ERROR:'//errstr
        WRITE(*,'(a/)',IOSTAT=ierr) 'Exiting '//model
        INQUIRE(UNIT=logunit,OPENED=openstat)
        IF (openstat) THEN
            WRITE(logunit,'(a)',IOSTAT=ierr) 'ERROR:'//errstr
            WRITE(logunit,'(a/)',IOSTAT=ierr) 'Exiting '//model
        END IF
        STOP
    ELSE
        IF (sub/=oldsub) THEN ! IF SUB IS DIFFERENT FROM LAST HIT
            errstr = trim(sub)//':'//errstr
            oldsub = sub
        END IF
    END IF
ENDIF
!$OMP END CRITICAL(c)

END SUBROUTINE errchk
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE warn(str)
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
CHARACTER(LEN=*),INTENT(IN) :: str
! LOCAL VARIABLES
INTEGER(KIND=ISGL),PARAMETER :: logunit = 100
CHARACTER(LEN=30) :: sub = 'warn'
INTEGER(KIND=ISGL) :: ierr
LOGICAL :: openstat

!$OMP CRITICAL(w)
Nwarn = Nwarn + 1
WRITE(*,'("WARNING: "a)',IOSTAT=ierr) TRIM(str)
INQUIRE(UNIT=logunit,OPENED=openstat)
IF (openstat) THEN
    WRITE(logunit,'("WARNING: "a)',IOSTAT=ierr) TRIM(str)
    IF (ierr /= 0) THEN
        CALL err(sub,'error writing to log file')
    END IF
END IF
!$OMP END CRITICAL(w)

END SUBROUTINE warn
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE errhold()
IMPLICIT NONE
!$OMP CRITICAL(h)
IF (errstat) held = .TRUE.
!$OMP END CRITICAL(h)
END SUBROUTINE errhold
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE errunhold()
IMPLICIT NONE
!$OMP CRITICAL(u)
held = .FALSE.
!$OMP END CRITICAL(u)
END SUBROUTINE errunhold
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE GEN_ERROR
