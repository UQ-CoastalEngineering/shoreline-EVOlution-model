! FVUTILGEO - FVWBM GEOMETRY UTILITY MODULE
!
! Copyright WBM 2007
! Authors: Ian Teakle

MODULE GEN_GEO

INCLUDE 'COMPILER_DIRECTIVES.FI'

! MODULE USE STATEMENTS
USE PRECISION

IMPLICIT NONE

! MODULE ACCESS STATEMENTS
PRIVATE
PUBLIC :: distance

INTERFACE distance
    MODULE PROCEDURE distance_r
!DEC$ IF (_PRECISION==1)
    MODULE PROCEDURE distance_d
!DEC$ END IF
END INTERFACE
! MODULE PROCEDURES
CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
FUNCTION distance_r(x1,y1,x2,y2) RESULT(dist)
! CALCULATE DISTANCE BETWEEN 2D POINTS

! FUNCTION DECLARATIONS
IMPLICIT NONE
! FUNCTION ARGUMENTS
REAL(KIND=SGL),INTENT(IN) :: x1
REAL(KIND=SGL),INTENT(IN) :: y1
REAL(KIND=SGL),INTENT(IN) :: x2
REAL(KIND=SGL),INTENT(IN) :: y2
! FUNCTION RESULT
REAL(KIND=SGL) :: dist

dist = sqrt((x2-x1)**2+(y2-y1)**2)

END FUNCTION distance_r
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DEC$ IF (_PRECISION==1)
FUNCTION distance_d(x1,y1,x2,y2) RESULT(dist)
! CALCULATE DISTANCE BETWEEN 2D POINTS

! FUNCTION DECLARATIONS
IMPLICIT NONE
! FUNCTION ARGUMENTS
REAL(KIND=DBL),INTENT(IN) :: x1
REAL(KIND=DBL),INTENT(IN) :: y1
REAL(KIND=DBL),INTENT(IN) :: x2
REAL(KIND=DBL),INTENT(IN) :: y2
! FUNCTION RESULT
REAL(KIND=DBL) :: dist

dist = sqrt((x2-x1)**2+(y2-y1)**2)

END FUNCTION distance_d
!DEC$ END IF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE GEN_GEO
