! FVGLOBAL - Module containing global variables for use in FVWBM and associated modules
!
! Copyright BMT WBM 2008
! Authors: Ian Teakle

MODULE globals
!!DEC$ STRICT

INCLUDE 'COMPILER_DIRECTIVES.FI'

! MODULE USE STATEMENTS
USE PRECISION

! MODULE DECLARATIONS
IMPLICIT NONE

! MODULE ACCESS
PUBLIC

! MODULE TYPE DEFINITIONS
TYPE global_ctrl_typ
    LOGICAL :: do_xs_update
    LOGICAL :: do_ls_update
    INTEGER(KIND=ISGL)  :: tform
    REAL(KIND=DBL) :: tzero
    REAL(KIND=DBL) :: tstart
    REAL(KIND=SGL) :: rhow
    REAL(KIND=SGL) :: rhos
    REAL(KIND=SGL) :: g
    REAL(KIND=SGL) :: porosity
    REAL(KIND=SGL) :: morfac
    REAL(KIND=SGL) :: breaker_index
    REAL(KIND=SGL),DIMENSION(2) :: omega
END TYPE

! MODULE PARAMETERS
REAL(KIND=SGL),PARAMETER :: pi = 4.0_SGL*ATAN(1.0_SGL)                ! PI WITH SGL PRECISION
REAL(KIND=DBL),PARAMETER :: pi_dbl = 4.0_DBL*ATAN(1.0_DBL)            ! PI WITH DBL PRECISION
REAL(KIND=SGL),PARAMETER :: deg_to_rad = pi / 180._SGL                ! DEGREES TO RADIANS SGL PRECISION
REAL(KIND=SGL),PARAMETER :: deg_to_rad_dbl = pi_dbl / 180._DBL        ! DEGREES TO RADIANS DBL PRECISION
REAL(KIND=SGL),PARAMETER :: rad_to_deg = 180._SGL / pi                ! RADIANS TO DEGREES SGL PRECISION
REAL(KIND=SGL),PARAMETER :: rad_to_deg_dbl = 180._DBL / pi_dbl        ! RADIANS TO DEGREES DBL PRECISION

! MODULE VARIABLES
CHARACTER(LEN=LSTR),PROTECTED :: path                                 ! CONTROL FILE RELATIVE PATH
CHARACTER(LEN=LSTR),PROTECTED :: evcfil                               ! CONTROL FILE NAME
CHARACTER(LEN=LSTR),PROTECTED :: evc                                  ! CONTROL FILE NAME WITHOUT EXTENSION
LOGICAL,PROTECTED :: do_xs_update                                     ! DO XS PROFILE UPDATE FLAG
LOGICAL,PROTECTED :: do_ls_update                                     ! DO LS TRANSPORT UPDATE FLAG
INTEGER(KIND=ISGL),PROTECTED :: nstep                                 ! CURRENT INTEGER TIMESTEP
INTEGER(KIND=ISGL),PROTECTED :: tform                                 ! INPUT/OUTPUT TIME FORMAT
REAL(KIND=DBL),PROTECTED :: t                                         ! CURRENT MODEL TIME
REAL(KIND=DBL),PROTECTED :: tzero                                     ! ZERO TIME (JULIAN SECONDS)
REAL(KIND=SGL),PROTECTED :: rhow                                      ! DENSITY WATER (KG/M^3)
REAL(KIND=SGL),PROTECTED :: rhos                                      ! DENSITY SEDIMENT (KG/M^3)
REAL(KIND=SGL),PROTECTED :: g                                         ! GRAVITATIONAL ACCELERATION (M/S^2)
REAL(KIND=SGL),PROTECTED :: porosity                                  ! POROSITY (-)
REAL(KIND=SGL),PROTECTED :: morfac                                    ! MORFAC (-)
REAL(KIND=SGL),PROTECTED :: breaker_index                             ! BREAKER INDEX (-)
REAL(KIND=SGL),PROTECTED :: kappa                                     ! PARAMETER DERIVED FROM BREAKER INDEX (-)
REAL(KIND=SGL),DIMENSION(2),PROTECTED :: omega                        ! ACTIVE TRANSPORT DISTRIBUTIONS

! MODULE PROCEDURES
CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE initglobals(ctrl)
! SETS INITIAL VALUES OF THE GLOBAL VARIABLES
!
! SUBROUTINE DECLARATIONS
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(global_ctrl_typ),INTENT(IN) :: ctrl
!
do_xs_update = ctrl%do_xs_update
do_ls_update = ctrl%do_ls_update
nstep = 0
tform = ctrl%tform
tzero = ctrl%tzero
t = ctrl%tstart
rhow = ctrl%rhow
rhos = ctrl%rhos
g = ctrl%g
porosity = ctrl%porosity
morfac = ctrl%morfac
breaker_index = ctrl%breaker_index
kappa = (3./8. * breaker_index**2) / (1. + 3./8. * breaker_index**2)
omega = (/0., 1./)
!
END SUBROUTINE initglobals
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE set_path(pathin)
IMPLICIT NONE
CHARACTER(LEN=*) :: pathin
path = pathin
END SUBROUTINE set_path
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE set_evcfil(evcfilin)
IMPLICIT NONE
CHARACTER(LEN=*) :: evcfilin
evcfil = evcfilin
END SUBROUTINE set_evcfil
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE set_evc(evcin)
IMPLICIT NONE
CHARACTER(LEN=*) :: evcin
evc = evcin
END SUBROUTINE set_evc
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE update_nstep()
IMPLICIT NONE
nstep = nstep + 1
END SUBROUTINE update_nstep
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE update_t(tnew)
IMPLICIT NONE
REAL(KIND=DBL),INTENT(IN) :: tnew
t = tnew
END SUBROUTINE update_t
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE globals
