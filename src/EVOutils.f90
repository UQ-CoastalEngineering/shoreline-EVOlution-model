! EVOmod
! Containing types, objects and procedures required by main program SHOREmod
!

MODULE EVOutils
!!DEC$ STRICT

INCLUDE 'COMPILER_DIRECTIVES.FI'

! MODULE USE STATEMENTS
USE PRECISION
USE GLOBALS
USE GEN_ERROR
USE LOG
!DEC$ IF (LICENSE==1)
USE LICENSE
!DEC$ END IF

! MODULE DECLARATIONS
IMPLICIT NONE

! MODULE PARAMETER DECLARATIONS

! MODULE OBJECT DECLARATIONS
CHARACTER(LEN=LSTR),ALLOCATABLE,DIMENSION(:) :: argument    ! PROGRAM ARGUMENTS
CHARACTER(LEN=LSTR) :: evcfull                              ! CONTROL FILE NAME INCL PATH AND EXTENSION
!TYPE(fvdomain) :: domain                                   ! DOMAIN OBJECT

! MODULE PROCEDURES
CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE getargs()
! GET PROGRAM ARGUMENTS

! SUBROUTINE USE STATEMENTS
USE GEN_FILE, ONLY : fileparts, fullfile

! SUBROUTINE DECLARATIONS
IMPLICIT NONE
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'getargs'
INTEGER(KIND=ISGL) :: Narg
INTEGER(KIND=ISGL) :: n
INTEGER(KIND=ISGL) :: alloc_stat
CHARACTER(LEN=LSTR) :: pathtmp, evctmp, exttmp

Narg=command_argument_count( )
IF (Narg==0) THEN
    Narg=1
    ALLOCATE(argument(Narg),STAT=alloc_stat)
    argument(1) = ''
    WRITE(*,'(a)') 'Enter input file:  '
    READ(*,'(a)') argument(1)
ELSE
    ALLOCATE(argument(Narg),STAT=alloc_stat)
    IF (alloc_stat /= 0) THEN
        CALL err(sub,'allocation error')
    END IF
    DO n = 1,Narg
        CALL get_command_argument(n, argument(n))
    END DO
END IF

SELECT CASE (Narg)
CASE (1)
    evcfull = argument(1)
    CALL fileparts(argument(1),PATH=pathtmp,NAME=evctmp,EXT=exttmp)
    CALL set_path(pathtmp)
    CALL set_evcfil(fullfile('',evctmp,exttmp))
    CALL set_evc(evctmp)
CASE DEFAULT
    CALL err(sub,'Incorrect argument specification')
END SELECT

END SUBROUTINE getargs
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE details()
! OUTPUT BUILD DETAILS

! SUBROUTINE DECLARATIONS
IMPLICIT NONE

! SPECIFY BUILD PARAMETERS
CHARACTER(LEN=20) :: build_version = '2016.01.000'          ! BUILD VERSION
CHARACTER(LEN=20) :: build_date = '26/01/2016 09:45'        ! BUILD DATE
!DEC$ IF (PLATFORM==1)                                      ! BUILD PLATFORM (SET IN 'COMPILER_DIRECTIVES.FI')
CHARACTER(LEN=20) :: build_platform = 'Windows'
CHARACTER(LEN=20) :: compiler_version = 'Ifort 16.0.1'    ! WINDOWS COMPILER VERSION
!DEC$ ELSEIF (PLATFORM==2)
CHARACTER(LEN=20) :: build_platform = 'Linux'
CHARACTER(LEN=20) :: compiler_version = 'Ifort 16.0.1'    ! LINUX COMPILER VERSION
!DEC$ END IF
!DEC$ IF (ARCH==1)                                          ! BUILD ARCHITECTURE (SET IN 'COMPILER_DIRECTIVES.FI')
CHARACTER(LEN=6) :: build_arch = 'x86-32'
!DEC$ ELSEIF (ARCH==2)
CHARACTER(LEN=6) :: build_arch = 'x86-64'
!DEC$ END IF
!DEC$ IF (_PRECISION==1)                                     ! BUILD PRECISION (SET IN 'COMPILER_DIRECTIVES.FI')
CHARACTER(LEN=6) :: build_precision = 'single'
!DEC$ ELSEIF (_PRECISION==2)
CHARACTER(LEN=6) :: build_precision = 'double'
!DEC$ END IF

! REPORT BUILD DETAILS
CALL message('****************************** EVO ******************************','(/a/)')
CALL message('Build version: '//TRIM(build_version),'(a)')
CALL message('Build date: '//TRIM(build_date),'(a)')
CALL message('Build platform: '//TRIM(build_platform),'(a)')
CALL message('Build architecture:  '//TRIM(build_arch),'(a)')
CALL message('Compiler version: '//TRIM(compiler_version),'(a)')
CALL message('Build precision: '//TRIM(build_precision),'(a)')
CALL message('*****************************************************************','(/a/)')

END SUBROUTINE details
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE cleanup()
! PERFORM CLEANUP

! SUBROUTINE USE STATEMENTS
USE GEN_FILE, ONLY : closeall
USE GEN_STRING, ONLY : num2str

! SUBROUTINE DECLARATIONS
IMPLICIT NONE
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'cleanup'

CALL message ('Performing final cleanup tasks:','(/a)')
! ECHO NUMBER OF WARNINGS
IF (Nwarn>0) CALL message('Number of warnings = '//num2str(Nwarn),'(/a/)')
! CLOSE ALL OPEN FILES
CALL closeall()
CALL errchk(sub); IF (errstat) RETURN
CALL message ('Successful.','(a/)')
! EXIT EVOMOD MESSAGE
CALL message('Exiting EVO','(a/)')
! CLOSE LOG FILE
CALL closelog()

END SUBROUTINE cleanup
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE EVOutils
