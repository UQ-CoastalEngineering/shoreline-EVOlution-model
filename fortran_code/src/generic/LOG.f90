! FVLOG - FVWBM LOG MODULE
!
! Copyright WBM 2007
! Authors: Ian Teakle

MODULE LOG
!!DEC$ STRICT

INCLUDE 'COMPILER_DIRECTIVES.FI'

! MODULE USE STATEMENTS
USE PRECISION
USE GEN_ERROR

! MODULE DECLARATIONS
IMPLICIT NONE

! MODULE ACCESS STATEMENTS
PRIVATE
PUBLIC :: logdir, logunit
PUBLIC :: openlog, message, closelog

! MODULE PARAMETERS
INTEGER(KIND=ISGL),PARAMETER :: logunit = 100       ! LOG FILE UNIT NUMBER

 ! MODULE VARIABLES
 CHARACTER(LEN=LSTR),PROTECTED :: logdir            ! LOG FILE DIRECTORY PATH

! MODULE PROCEDURES
CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE openlog(fvcfull,path,fvc)

! SUBROUTINE USE STATEMENTS
USE GEN_STRING, ONLY : iscomment, removecomments, parse, strrep, upper
! SUBROUTINE DECLARATIONS
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
CHARACTER(LEN=*),INTENT(IN) :: fvcfull
CHARACTER(LEN=*),INTENT(IN) :: path
CHARACTER(LEN=*),INTENT(IN) :: fvc
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'openlog'
CHARACTER(LEN=LSTR) :: logname, line, token, remain
INTEGER(KIND=ISGL) :: ierr
LOGICAL :: exist

! DETERMINE IMPLICIT LOG DIRECTORY
INQUIRE(DIRECTORY=TRIM(path)//'log',EXIST=exist)
IF (exist) THEN
    logdir = TRIM(path)//'log'
ELSE
    logdir = TRIM(path)
END IF
! HAVE A LOOK IN CONTROL FILE TO SEE IF LOGDIR HAS BEEN SPECIFIED EXPLICITLY
OPEN(UNIT=10,FILE=fvcfull,STATUS='old',ACTION='read',IOSTAT=ierr)
IF (ierr == 0) THEN
    DO
        READ(10,'(a)',IOSTAT=ierr) line
        IF (ierr /= 0) EXIT
        line = ADJUSTL(line)
        IF (iscomment(line)) CYCLE
        line = removecomments(line)
        CALL parse(line,'==',token,remain)
        token = strrep(token,' ','')
        token = upper(token)
        IF (token=='LOGDIR') THEN
            logdir = remain
        END IF
    END DO
END IF
CLOSE(10)
! OPEN LOG FILE
IF (TRIM(fvc)/='') THEN
    logname = fullfile(logdir,fvc,'.log')
ELSE
    logname = fullfile(logdir,'evomod','.log')
END IF
OPEN(UNIT=logunit,FILE=logname,STATUS='replace',ACTION='write',FORM='formatted',IOSTAT=ierr)
IF (ierr /= 0) THEN
    CALL err(sub,'error opening log file, '//TRIM(logname)); RETURN
END IF

END SUBROUTINE openlog
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE message(str,fmt)
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
CHARACTER(LEN=*),INTENT(IN) :: str
CHARACTER(LEN=*),INTENT(IN) :: fmt
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'message'
INTEGER(KIND=ISGL) :: ierr
LOGICAL :: openstat

WRITE(*,fmt,IOSTAT=ierr) TRIM(str)
INQUIRE(UNIT=logunit,OPENED=openstat)
IF (openstat) THEN
    WRITE(logunit,fmt,IOSTAT=ierr) TRIM(str)
    IF (ierr /= 0) THEN
        CALL err(sub,'error writing to log file'); RETURN
    END IF
    FLUSH(logunit,IOSTAT=ierr)
END IF

END SUBROUTINE message
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE closelog()
IMPLICIT NONE
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'closelog'
INTEGER(KIND=ISGL) :: ierr

CLOSE(logunit,IOSTAT=ierr)
IF (ierr /= 0) THEN
    CALL err(sub,'error closing log file'); RETURN
END IF

END SUBROUTINE closelog
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PURE FUNCTION fullfile(path,name,ext)
IMPLICIT NONE
! FUNCTION ARGUMENTS
CHARACTER(LEN=*),INTENT(IN) :: path
CHARACTER(LEN=*),INTENT(IN) :: name
CHARACTER(LEN=*),INTENT(IN) :: ext
! FUNCTION RESULT
CHARACTER(LEN=LEN_TRIM(path)+LEN_TRIM(name)+LEN_TRIM(ext)+1) :: fullfile
! LOCAL VARIABLES
CHARACTER(LEN=LEN(name)) :: tmp
CHARACTER(LEN=1) :: separator
LOGICAL :: absolute_path
INTEGER(KIND=ISGL) :: i, j

tmp = TRIM(ADJUSTL(name))
!DEC$ IF (PLATFORM==1)
separator = '\'
IF (tmp(1:1)=='\'.OR.tmp(1:1)=='/'.OR.tmp(2:3)==':\'.OR.tmp(2:3)==':/') THEN
    absolute_path = .TRUE.
ELSE
    absolute_path = .FALSE.
END IF
!DEC$ ELSE
separator = '/'
IF (tmp(1:1)=='/') THEN
    absolute_path = .TRUE.
ELSE
    absolute_path = .FALSE.
END IF
!DEC$ END IF
i = INDEX(path,separator,.TRUE.)
IF (absolute_path) THEN
    fullfile = TRIM(ADJUSTL(name))//TRIM(ADJUSTL(ext))
ELSE
    IF (i==LEN_TRIM(path)) THEN
        fullfile = TRIM(ADJUSTL(path))//TRIM(ADJUSTL(name))//TRIM(ADJUSTL(ext))
    ELSE
        fullfile = TRIM(ADJUSTL(path))//separator//TRIM(ADJUSTL(name))//TRIM(ADJUSTL(ext))
    END IF
END IF

END FUNCTION fullfile
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE LOG