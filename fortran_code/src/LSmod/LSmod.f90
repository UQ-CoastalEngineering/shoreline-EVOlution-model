! LONGSHORE SHORELINE EVOLUTION MODULE

MODULE LSmod

! MODULE USE STATEMENTS
USE PRECISION
USE GEN_ERROR
USE LOG
USE GLOBALS
USE WVmod, ONLY : WvParTyp
USE OUTmod, ONLY : OutCtrlTyp, GenOutTyp

! MODULE DECLARATIONS
IMPLICIT NONE

! MODULE ACCESS
PRIVATE
PUBLIC :: LSmodtyp, LSparamtyp, Groynetyp, construct, destruct, set_parameters, initialise, update, output

! MODULE INTERFACES
INTERFACE construct
    MODULE PROCEDURE ls_construct
END INTERFACE
INTERFACE destruct
    MODULE PROCEDURE ls_destruct
END INTERFACE
INTERFACE set_parameters
    MODULE PROCEDURE ls_set_parameters
END INTERFACE
INTERFACE initialise
    MODULE PROCEDURE ls_initialise
END INTERFACE
INTERFACE update
    MODULE PROCEDURE ls_update
END INTERFACE
INTERFACE calc_coords
    MODULE PROCEDURE calc_coords_s
    MODULE PROCEDURE calc_coords_v
END INTERFACE
INTERFACE output
    MODULE PROCEDURE ls_output
END INTERFACE

! GROYNE SUB-TYPE
TYPE Groynetyp
    INTEGER(KIND=ISGL) :: id
    REAL(KIND=SGL) :: chain
    REAL(KIND=SGL) :: xg
    REAL(KIND=DBL) :: start_time
    REAL(KIND=DBL) :: end_time
END TYPE
! LS MODEL PARAMETER SUB-TYPE
TYPE LSparamtyp
    REAL(KIND=SGL) :: K1                                            ! CERC formula non-dimensional coefficient
END TYPE
! LS MODEL TYPE
TYPE LSmodtyp
    INTEGER(KIND=ISGL) :: Ny                                        ! Number of profiles in longshore model
    INTEGER(KIND=ISGL) :: Nl                                        ! Number of lines in longshore model
    INTEGER(KIND=ISGL) :: Ng                                        ! Number of groynes in longshore model
    INTEGER(KIND=ISGL) :: Nout                                      ! Number of output requests
    TYPE(LSparamtyp) :: param                                       ! Longshore model parameters
    REAL(KIND=DBL),ALLOCATABLE,DIMENSION(:,:) :: base               ! Baseline coordinates (mEast,mNorth) [2,Ny]
    REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:) :: chain                ! Baseline chainages (m) [Ny]
    REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:) :: eta                  ! Local coordinate rotation (rad) [Ny]
    REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:) :: dy                   ! Base dy (m) [Ny-1]
    REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:,:) :: lcv                ! Base local coordinate values (zeta1,chi1,psi1,zeta2,chi2,psi2) [Ny-2]
    REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:,:) :: xl                 ! Line x position [Nl,Ny-1]
    REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:) :: x0                   ! Shoreline x position [Ny-1]
    REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:) :: x0a                  ! Apparent shoreline x position [Ny-1]
    REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:) :: xbr                  ! Breaking wave x position [Ny-1]
    TYPE(WvPartyp),ALLOCATABLE,DIMENSION(:) :: wv_brk               ! Breaking wave parameters [Ny]
    REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:,:) :: theta              ! Face contour angle in local coordinates (rad) [Nl,Ny]
    REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:) :: thetab               ! Effective contour angle in local coordinates (rad) [Ny]             
    REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:) :: alphab               ! Breaking wave angle in local coordinates (rad) [Ny]
    REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:) :: Qspot                ! Potential longshore transport rate (m^3/s) [Ny]
    REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:) :: Qs                   ! Longshore transport rate (m^3/s) [Ny]
    TYPE(Groynetyp),ALLOCATABLE,DIMENSION(:) :: gr                  ! Groyne objects [Ng]
    TYPE(GenOutTyp),ALLOCATABLE,DIMENSION(:) :: out                 ! Longshore model output type [Nout]
END TYPE

! MODULE VARIABLES

! MODULE PROCEDURES
CONTAINS
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE ls_construct(LS,Nl,base,eta,groynes,output)
! LS OBJECT CONSTRUCTOR ROUTINE
USE GEN_GEO, ONLY : distance
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(LSmodtyp),INTENT(OUT) :: LS
INTEGER(KIND=ISGL),INTENT(IN) :: Nl
REAL(KIND=DBL),DIMENSION(:,:),INTENT(IN) :: base
REAL(KIND=SGL),DIMENSION(:),INTENT(IN) :: eta
TYPE(Groynetyp),DIMENSION(:),OPTIONAL,INTENT(IN) :: groynes
TYPE(OutCtrlTyp),DIMENSION(:),OPTIONAL,INTENT(IN) :: output
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'ls_construct'
INTEGER(KIND=ISGL) :: i, Ny
INTEGER(KIND=ISGL) :: tmpstat, alloc_stat
REAL(KIND=DBL) :: dy

Ny = SIZE(base,2)
IF (SIZE(eta)/=Ny) THEN
    CALL err(sub,'Input arguments ''base'' & ''eta'' are not size compatible.')
END IF
alloc_stat = 0
ALLOCATE(LS%base(2,Ny), STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
ALLOCATE(LS%chain(Ny), STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
ALLOCATE(LS%eta(Ny), STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
ALLOCATE(LS%dy(Ny-1), STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
ALLOCATE(LS%lcv(6,Ny-2), STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
ALLOCATE(LS%xl(Nl,Ny-1), STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
ALLOCATE(LS%x0(Ny-1), STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
ALLOCATE(LS%x0a(Ny-1), STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
ALLOCATE(LS%xbr(Ny-1), STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
ALLOCATE(LS%Wv_brk(Ny), STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
ALLOCATE(LS%theta(Nl,Ny), STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
ALLOCATE(LS%thetab(Ny), STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
ALLOCATE(LS%alphab(Ny), STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
ALLOCATE(LS%Qspot(Ny), STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
ALLOCATE(LS%Qs(Ny), STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
IF (alloc_stat/=0) THEN
    CALL err(sub,'Allocate error'); RETURN
END IF
! INITIALISE VALUES
LS%Ny = Ny
LS%NL = Nl
LS%Ng = 0
LS%Nout = 0
! FACE COORDINATES
LS%base = base
! FACE CHAINAGE
LS%chain(1) = 0.
DO i = 2,Ny
    dy = distance(base(1,i),base(2,i),base(1,i-1),base(2,i-1))
    LS%dy(i-1) = dy
    LS%chain(i) = LS%chain(i-1) + dy
END DO
! ETA (FACE LOCAL COORDINATE ROTATION)
LS%eta = eta
! BASE LOCAL COORDINATE VALUES
CALL construct_lcv()
! BREAKING WAVES
DO i=1,Ny
    LS%wv_brk%dpth = 0.
    LS%wv_brk%Per = 0.
    LS%wv_brk%Ht = 0.
    LS%wv_brk%Dir = 0.
END DO
LS%theta = 0.
LS%thetab = 0.
LS%alphab = 0.
! SEDIMENT FLUX
LS%Qspot = 0.
LS%Qs = 0.
! GROYNES (IF REQUESTED)
CALL construct_groynes()
CALL construct_output()

CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE construct_lcv()

! LOCAL VARIABLES
REAL(KIND=SGL) :: eta1, chi1, psi1, eta2, chi2, psi2
REAL(KIND=DBL) :: x1, y1, x2, y2

DO i = 1,Ny-2
    !
    eta1 = 0.5*(eta(i)+eta(i+1))
    x1 = 0.5_DBL*(base(1,i)+base(1,i+1))
    y1 = 0.5_DBL*(base(2,i)+base(2,i+1))
    chi1 = REAL(x1 - base(1,i+1), KIND=SGL)
    psi1 = REAL(y1 - base(2,i+1), KIND=SGL)
    !
    eta2 = 0.5*(eta(i+1)+eta(i+2))
    x2 = 0.5_DBL*(base(1,i+1)+base(1,i+2))
    y2 = 0.5_DBL*(base(2,i+1)+base(2,i+2))
    chi2 = REAL(x2 - base(1,i+1), KIND=SGL)
    psi2 = REAL(y2 - base(2,i+1), KIND=SGL)
    !
    ls%lcv(1,i) = eta1
    ls%lcv(2,i) = chi1
    ls%lcv(3,i) = psi1
    ls%lcv(4,i) = eta2
    ls%lcv(5,i) = chi2
    ls%lcv(6,i) = psi2
END DO

END SUBROUTINE construct_lcv
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE construct_groynes()
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: Ng, k
REAL(KIND=SGL) :: tmpchain, d1, d2

IF (.NOT.PRESENT(groynes)) RETURN
Ng = SIZE(groynes)
IF (Ng==0) RETURN
ALLOCATE(LS%gr(Ng), STAT=alloc_stat)
IF (alloc_stat/=0) THEN
    CALL err(sub,'Allocate error'); RETURN
END IF
LS%Ng = Ng
DO i = 1,Ng
    ! Find closest longshore grid point
    tmpchain = groynes(i)%chain
    
    k=minloc(ABS(LS%chain-tmpchain),DIM=1)
    LS%gr(i)%id = k

    ! Set groyne position
    LS%gr(i)%chain = groynes(i)%chain
    LS%gr(i)%xg = groynes(i)%xg
    ! Set start and end time
    LS%gr(i)%start_time = groynes(i)%start_time
    LS%gr(i)%end_time = groynes(i)%end_time
END DO

END SUBROUTINE construct_groynes
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE construct_output()
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: Ntmp, Nout, i

IF (.NOT.PRESENT(output)) RETURN
Ntmp = SIZE(output,1)
IF (Ntmp==0) RETURN
! Count LS model related outputs
Nout = 0
DO i = 1,Ntmp
    SELECT CASE (output(i)%typnam)
    CASE('SHORELINE','TRANSPORT','LONGSHORE')
        Nout = Nout + 1
    END SELECT
END DO
IF (Nout==0) RETURN

! Allocate wv%out
ALLOCATE(ls%out(Nout),STAT=alloc_stat)
IF (alloc_stat/=0) THEN
    CALL err(sub, 'Allocate error.'); RETURN
END IF
! Initialise values
LS%Nout = Nout
Nout = 0
DO i = 1,Ntmp
    SELECT CASE (output(i)%typnam)
    CASE('SHORELINE')
        Nout = Nout + 1
        LS%out(Nout)%typ = 1
    CASE('LONGSHORE')
        Nout = Nout + 1
        LS%out(Nout)%typ = 2
    CASE('TRANSPORT')
        Nout = Nout + 1
        LS%out(Nout)%typ = 3
        alloc_stat = 0
        ALLOCATE(LS%out(Nout)%dat(3), STAT=tmpstat); IF (tmpstat/=0) alloc_stat=tmpstat
        IF (alloc_stat/=0) THEN
            CALL err(sub, 'Allocate error.'); RETURN
        END IF
        LS%out(Nout)%dat(3)%name = 'time-integration span'
        ALLOCATE(ls%out(Nout)%dat(1)%x(1), STAT=tmpstat); IF (tmpstat/=0) alloc_stat=tmpstat
        LS%out(Nout)%dat(2)%name = 'time-integrated Qs'
        ALLOCATE(ls%out(Nout)%dat(2)%x(LS%Ny), STAT=tmpstat); IF (tmpstat/=0) alloc_stat=tmpstat
        LS%out(Nout)%dat(2)%name = 'time-integrated Qspot'
        ALLOCATE(ls%out(Nout)%dat(3)%x(LS%Ny), STAT=tmpstat); IF (tmpstat/=0) alloc_stat=tmpstat
        IF (alloc_stat/=0) THEN
            CALL err(sub, 'Allocate error.'); RETURN
        END IF
        LS%out(Nout)%dat(1)%x = 0.
        LS%out(Nout)%dat(2)%x = 0.
    CASE DEFAULT
        CYCLE
    END SELECT
    ls%out(Nout)%OutCtrlTyp = output(i)
    ls%out(Nout)%fid = 0    
    ls%out(Nout)%init = .FALSE.
    ls%out(Nout)%t = output(i)%tstart
END DO

END SUBROUTINE construct_output
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE ls_construct
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE ls_destruct(LS)
USE GEN_FILE, ONLY : closefile
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(LSmodtyp),INTENT(INOUT) :: LS
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'ls_destruct'
INTEGER(KIND=ISGL) :: i, tmpstat, dealloc_stat

dealloc_stat = 0
DO i = 1,SIZE(LS%out)
    CALL closefile(UNIT=LS%out(i)%fid)
    CALL errchk(sub); IF (errstat) RETURN
END DO
DEALLOCATE(LS%base, STAT=tmpstat); IF (tmpstat/=0) dealloc_stat = tmpstat
DEALLOCATE(LS%chain, STAT=tmpstat); IF (tmpstat/=0) dealloc_stat = tmpstat
DEALLOCATE(LS%eta, STAT=tmpstat); IF (tmpstat/=0) dealloc_stat = tmpstat
DEALLOCATE(LS%dy, STAT=tmpstat); IF (tmpstat/=0) dealloc_stat = tmpstat
DEALLOCATE(LS%lcv, STAT=tmpstat); IF (tmpstat/=0) dealloc_stat = tmpstat
DEALLOCATE(LS%xl, STAT=tmpstat); IF (tmpstat/=0) dealloc_stat = tmpstat
DEALLOCATE(LS%x0, STAT=tmpstat); IF (tmpstat/=0) dealloc_stat = tmpstat
DEALLOCATE(LS%x0a, STAT=tmpstat); IF (tmpstat/=0) dealloc_stat = tmpstat
DEALLOCATE(LS%xbr, STAT=tmpstat); IF (tmpstat/=0) dealloc_stat = tmpstat
DEALLOCATE(LS%Wv_brk, STAT=tmpstat); IF (tmpstat/=0) dealloc_stat = tmpstat
DEALLOCATE(LS%theta, STAT=tmpstat); IF (tmpstat/=0) dealloc_stat = tmpstat
DEALLOCATE(LS%thetab, STAT=tmpstat); IF (tmpstat/=0) dealloc_stat = tmpstat
DEALLOCATE(LS%alphab, STAT=tmpstat); IF (tmpstat/=0) dealloc_stat = tmpstat
DEALLOCATE(LS%Qspot, STAT=tmpstat); IF (tmpstat/=0) dealloc_stat = tmpstat
DEALLOCATE(LS%Qs, STAT=tmpstat); IF (tmpstat/=0) dealloc_stat = tmpstat
DEALLOCATE(LS%out, STAT=tmpstat); IF (tmpstat/=0) dealloc_stat = tmpstat
LS%Ny = 0
LS%NL = 0

END SUBROUTINE ls_destruct 
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE ls_set_parameters(ls,param)
! SET LS MODEL PARAMETERS
IMPLICIT NONE
TYPE(LSmodtyp),INTENT(INOUT) :: ls
TYPE(LSparamtyp),INTENT(IN) :: param
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'ls_set_parameters'

ls%param%K1 = param%K1 ! CERC formula non-dimensional coefficient

END SUBROUTINE ls_set_parameters
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE ls_initialise(LS,xl_init)
! INITIALISE LS OBJECT
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(LSmodtyp),INTENT(INOUT) :: LS
REAL(KIND=SGL),DIMENSION(:,:),INTENT(IN) :: xl_init
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'ls_initialise'

IF (SIZE(xl_init,1)/=LS%Nl .OR. SIZE(xl_init,2)/=LS%Ny-1) THEN
    CALL err(sub,'Input argument ''xl_init'' must be size compatible with LS object.')
    RETURN
END IF
LS%xl = xl_init
CALL calc_theta(LS)

END SUBROUTINE ls_initialise
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE calc_theta(LS)
! CALCULATE CONTOUR ROTATION (LOCAL COORDINATES)
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(LSmodtyp),INTENT(INOUT) :: LS
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'ls_calc_theta'
INTEGER(KIND=ISGL) :: i, j, Ny, Nl
REAL(KIND=SGL) :: xx, yy

Ny = LS%Ny
Nl = LS%Nl
! Calculate theta, which is contour rotation in local coordinates
DO i = 2,Ny-1
    DO j = 1,Nl
        LS%theta(j,i) = theta_func( LS%eta(i),                      & ! eta0
                                    LS%lcv(1,i-1),                  & ! eta1
                                    LS%lcv(2,i-1),                  & ! chi1
                                    LS%lcv(3,i-1),                  & ! psi1
                                    LS%xl(j,i-1),                   & ! x1
                                    LS%lcv(4,i-1),                  & ! eta2
                                    LS%lcv(5,i-1),                  & ! chi2
                                    LS%lcv(6,i-1),                  & ! psi2
                                    LS%xl(j,i)                      ) ! x2
    END DO
END DO
LS%theta(:,1) = LS%theta(:,2)
LS%theta(:,Ny) = LS%theta(:,Ny-1)
! Calculate effective contour rotation in local coordinates
LS%thetab = SUM(LS%theta,DIM=1) / REAL(Nl,KIND=SGL)

CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
FUNCTION theta_func(eta0,eta1,chi1,psi1,x1,eta2,chi2,psi2,x2) RESULT(theta)
IMPLICIT NONE
REAL(KIND=SGL),INTENT(IN) :: eta0, eta1, chi1, psi1, x1, eta2, chi2, psi2, x2
REAL(KIND=SGL) :: theta
! LOCAL PARAMETERS
REAL(KIND=SGL),PARAMETER :: pi_on_2 = pi / 2.
REAL(KIND=SGL),PARAMETER :: two_pi = 2. * pi
! LOCAL VARIABLES
REAL(KIND=SGL) :: chi11, psi11, chi22, psi22, dx, dy, beta
!
chi11 = chi1 + x1 * cos(eta1)
psi11 = psi1 + x1 * sin(eta1)
chi22 = chi2 + x2 * cos(eta2)
psi22 = psi2 + x2 * sin(eta2)
dx = chi22 - chi11
dy = psi22 - psi11
beta = atan2(dy,dx)
theta = beta - eta0 - pi_on_2
IF (theta < -pi) THEN
    theta = theta + two_pi
ELSE IF (theta > pi) THEN
    theta = theta - two_pi
END IF
!
END FUNCTION theta_func
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE calc_theta
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
PURE FUNCTION cerc(K1,db,Hb,Alpb,T) RESULT(Qsy)
! CALCULATE LONGSHORE TRANSPORT RATE (M^3/S) USING CERC FORMULA
USE GEN_UTIL, ONLY : kh_calc
IMPLICIT NONE
! FUNCTION ARGUMENTS
REAL(KIND=SGL),INTENT(IN) :: K1, db, Hb, Alpb, T
! FUNCTION RESULT
REAL(KIND=SGL) :: Qsy
! FUNCTION PARAMETERS
REAL(KIND=SGL),PARAMETER :: Alpb_lim = pi / 4.0_SGL
! LOCAL VARIABLES
REAL(KIND=SGL) :: a1, sgn, absAlpb, kho, khb, Cgb, ECgb

a1 = K1 / ((rhos-rhow)*g*(1.-porosity))
sgn = sign(1.,Alpb) 
absAlpb = abs(Alpb)
absAlpb = MIN(absAlpb,Alpb_lim)
kho = 4. * pi**2 / g / T**2 * db
khb = kh_calc(kho)
Cgb = 0.5 * (g * T / (2.*pi) * tanh(khb)) * (1. + 2. * khb / sinh(2.*khb))
ECgb = 1./8. * rhow * g * Hb**2 * Cgb
Qsy = sgn * a1 * ECgb * sin(absAlpb) * cos(absAlpb)

END FUNCTION cerc
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE calc_potential_transport(LS)
! CALCULATE POTENTIAL LONGSHORE TRANSPORT RATE
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(LSmodtyp),INTENT(INOUT) :: LS
! LOCAL PARAMETERS
REAL(KIND=SGL),PARAMETER :: pi_on_2 = pi / 2._SGL
REAL(KIND=SGL),PARAMETER :: pi_on_4 = pi / 4._SGL
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: i

! Calculate potential longshore transport rate
DO i = 2,LS%Ny-1
    LS%alphab(i) = LS%wv_brk(i)%dir + LS%eta(i) + LS%thetab(i) - pi_on_2 ! Breaking wave angle in local coordinates
    IF (LS%alphab(i)<-pi) THEN
        LS%alphab(i) = LS%alphab(i) + 2. * pi
    ELSE IF (LS%alphab(i)>pi) THEN
        LS%alphab(i) = LS%alphab(i) - 2. * pi
    END IF
    LS%Qspot(i) = cerc( LS%param%K1,        & ! K1
                        LS%wv_brk(i)%dpth,  & ! db
                        LS%wv_brk(i)%Ht,    & ! Hb
                        LS%alphab(i),       & ! Alpb
                        LS%wv_brk(i)%Per    ) ! T
    IF (LS%alphab(i) > pi_on_4) THEN
        IF (LS%Qspot(i)<LS%Qspot(i-1)) THEN
            LS%Qspot(i) = LS%Qspot(i-1)
        END IF
    ELSE IF (LS%alphab(i) < -pi_on_4) THEN
        IF (LS%Qspot(i)>LS%Qspot(i+1)) THEN
            LS%Qspot(i) = LS%Qspot(i+1)
        END IF
    END IF
END DO

END SUBROUTINE calc_potential_transport
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE calc_actual_transport(LS)
! CALCULATE ACTUAL LONGSHORE TRANSPORT RATE
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(LSmodtyp),INTENT(INOUT) :: LS
! LOCAL PARAMETERS
REAL(KIND=SGL),PARAMETER :: small = EPSILON(0.)
LOGICAL,PARAMETER :: filter_on = .TRUE.
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: i, iup
REAL(KIND=SGL) :: xfrac, Qsfrac

! Calculate actual longshore transport
DO i = 2,LS%Ny-1
    IF (LS%Qspot(i)>0.) THEN
        iup = i - 1
    ELSE
        iup = i
    END IF
    xfrac = (LS%xbr(iup) - LS%x0(iup)) / MAX(LS%xbr(iup) - LS%x0a(iup), small)
    xfrac = MAX(MIN(xfrac,1.),0.)
    IF (xfrac<0.05) xfrac = 0.
    Qsfrac = (omega(2) * (2. - xfrac) + omega(1) * xfrac) * xfrac / (omega(1) + omega(2))
    LS%Qs(i) = LS%Qspot(i) * Qsfrac
END DO

! Apply default zero-gradient boundary conditions
LS%Qs(1) = LS%Qs(2)
LS%Qs(LS%Ny) = LS%Qs(LS%Ny-1)

END SUBROUTINE calc_actual_transport
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE calc_groyne_transport(LS)
! CALCULATE GROYNE TRANSPORT RATE
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(LSmodtyp),INTENT(INOUT) :: LS
! LOCAL PARAMETERS
REAL(KIND=SGL),PARAMETER :: small = EPSILON(0.)
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: n, i
REAL(KIND=SGL) :: xg, Qspot, Qg, Qgfrac, Qs
REAL(KIND=SGL) :: QsL, xfracL, QfracL, QgL
REAL(KIND=SGL) :: QsR, xfracR, QfracR, QgR

DO n = 1,LS%Ng
    IF (t < LS%gr(n)%start_time .OR. t > LS%gr(n)%end_time) CYCLE
    i = LS%gr(n)%id
    xg = LS%gr(n)%xg
    Qspot = LS%Qs(i)
    ! Left side (i-1)
    QsL = LS%Qs(i-1)
    xfracL = (LS%xbr(i-1) - xg) / MAX(LS%xbr(i-1) - LS%x0(i-1), small)
    xfracL = MAX(MIN(xfracL,1.),0.)
    QfracL = (omega(2) * (2. - xfracL) + omega(1) * xfracL) * xfracL / (omega(1) + omega(2))
    QgL = MAX(QsL,0.) * QfracL
    ! Right side (i+1)
    QsR = LS%Qs(i+1)
    xfracR = (LS%xbr(i) - xg) / MAX(LS%xbr(i) - LS%x0(i), small)
    xfracR = MAX(MIN(xfracR,1.),0.)
    QfracR = (omega(2) * (2. - xfracR) + omega(1) * xfracR) * xfracR / (omega(1) + omega(2))
    QgR = MIN(QsR,0.) * QfracR
    Qg = QgL + QgR
    IF (Qspot>0.) THEN
        IF (xfracL<0.95) THEN
            Qs = Qg
        ELSE
            Qgfrac = (1.-xfracL)/0.05
            Qs = Qgfrac * Qg + (1.-Qgfrac) * Qspot
        END IF
        Qs = MAX(MIN(Qs,Qspot),0.)
    ELSE
        IF (xfracR<0.95) THEN
            Qs = Qg
        ELSE
            Qgfrac = (1.-xfracR)/0.05
            Qs = Qgfrac * Qg + (1.-Qgfrac) * Qspot
        END IF
        Qs = MIN(MAX(Qs,Qspot),0.)
    END IF  
    LS%Qs(i) = Qs
END DO

END SUBROUTINE calc_groyne_transport
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE filter_transport(LS)
! FILTER RESULTS FOR "ANTI-DIFFUSIVE" TRANSPORT GRADIENTS
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(LSmodtyp),INTENT(INOUT) :: LS
! LOCAL PARAMETERS
LOGICAL,PARAMETER :: filter_on = .FALSE.
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: i

IF (filter_on) THEN
    ! Sweep up the grid looking at positive Qs
    DO i = 3,LS%Ny-1
        IF (LS%alphab(i)>LS%alphab(i-1)) THEN
            IF (LS%Qs(i)>0. .AND. LS%Qs(i)<LS%Qs(i-1)) THEN
                LS%Qs(i) = LS%Qs(i-1)
            END IF
        END IF
    END DO
    ! Sweep down the grid lookint at negative Qs
    DO i = LS%Ny-2,2,-1
        IF (LS%alphab(i)<LS%alphab(i+1)) THEN
            IF (LS%Qs(i)<0. .AND. LS%Qs(i)>LS%Qs(i+1)) THEN
                LS%Qs(i) = LS%Qs(i+1)
            END IF
        END IF
    END DO
END IF

END SUBROUTINE filter_transport
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE apply_bcs(LS,bc)
! APPLY BOUNDARY CONDITIONS
USE BCmod, ONLY : BCtyp
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(LSmodTyp),INTENT(INOUT) :: LS
TYPE(BCtyp),DIMENSION(:),INTENT(IN) :: bc
! LOCAL PARAMETERS
REAL(KIND=SGL) :: convert_transport = 365.25 * 24. * 3600.

! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: i, id

DO i=1,SIZE(bc)
    SELECT CASE(bc(i)%typ)
    CASE (3) ! TRANSPORT
        id = bc(i)%obj(1)%id(1)
        LS%Qs(id) = bc(i)%tdat(1) / convert_transport
    END SELECT
END DO

END SUBROUTINE apply_bcs
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE ls_update(LS,wvbrk,xl,x0,x0a,xbr,bc)
! UPDATE LS OBJECT
USE BCmod, ONLY : BCtyp
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(LSmodTyp),INTENT(INOUT) :: LS
TYPE(WvParTyp),DIMENSION(:),INTENT(IN) :: wvbrk
REAL(KIND=SGL),DIMENSION(:,:),INTENT(IN) :: xl
REAL(KIND=SGL),DIMENSION(:),INTENT(IN) :: x0
REAL(KIND=SGL),DIMENSION(:),INTENT(IN) :: x0a
REAL(KIND=SGL),DIMENSION(:),INTENT(IN) :: xbr
TYPE(BCtyp),DIMENSION(:),INTENT(IN) :: bc
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'ls_update'
INTEGER(KIND=ISGL) :: i

! Check input argument sizes
IF (SIZE(wvbrk)/=LS%Ny) THEN
    CALL err(sub,'Input argument ''wvbrk'' must be size compatible with LS object.'); RETURN
END IF
IF (SIZE(xl,1)/=LS%Nl .OR. SIZE(xl,2)/=LS%Ny-1) THEN
    CALL err(sub,'Input argument ''xl'' must be size compatible with LS object.'); RETURN
END IF
IF (SIZE(x0)/=LS%Ny-1) THEN
    CALL err(sub,'Input argument ''x0'' must be size compatible with LS object.'); RETURN
END IF
IF (SIZE(x0a)/=LS%Ny-1) THEN
    CALL err(sub,'Input argument ''x0a'' must be size compatible with LS object.'); RETURN
END IF
IF (SIZE(xbr)/=LS%Ny-1) THEN
    CALL err(sub,'Input argument ''xbr'' must be size compatible with LS object.'); RETURN
END IF

! Update cross-shore position array
LS%xl = xl
! Update actual shoreline vector
LS%x0 = x0
! Update apparenet shoreline vector
LS%x0a = x0a
! Update breaking wave position vector
LS%xbr = xbr
! Update breaking wave conditions
DO i = 1,LS%Ny
    LS%wv_brk(i) = wvbrk(i)
END DO 
! Calculate contour angle
CALL calc_theta(LS)
! Calculate potential longshore transport
CALL calc_potential_transport(LS)
! Calculate actual longshore transport
CALL calc_actual_transport(LS)
! Calculate groyne transport
CALL calc_groyne_transport(LS)
! Filter transport gradients
CALL filter_transport(LS)
! Apply boundary transport conditions
CALL apply_bcs(LS,bc)

END SUBROUTINE ls_update
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
PURE FUNCTION calc_coords_s(base,eta,x) RESULT(coords)
! CALCULATE GLOBAL COORDINATES
IMPLICIT NONE
REAL(KIND=DBL),DIMENSION(2),INTENT(IN) :: base
REAL(KIND=SGL),INTENT(IN) :: eta
REAL(KIND=SGL),INTENT(IN) :: x
! FUNCTION RESULT
REAL(KIND=DBL),DIMENSION(2) :: coords

coords(1) = base(1) + REAL(x * cos(eta), KIND=DBL)
coords(2) = base(2) + REAL(x * sin(eta), KIND=DBL)

END FUNCTION calc_coords_s
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
PURE FUNCTION calc_coords_v(base,eta,x) RESULT(coords)
! CALCULATE GLOBAL COORDINATES
IMPLICIT NONE
REAL(KIND=DBL),DIMENSION(:,:),INTENT(IN) :: base
REAL(KIND=SGL),DIMENSION(:),INTENT(IN) :: eta
REAL(KIND=SGL),DIMENSION(:),INTENT(IN) :: x
! FUNCTION RESULT
REAL(KIND=DBL),DIMENSION(SIZE(base,1),SIZE(base,2)) :: coords
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: i, Ny

Ny = SIZE(base,2)
DO i = 1,Ny
    coords(:,i) = calc_coords_s(base(:,i),eta(i),x(i))
END DO

END FUNCTION calc_coords_v
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE ls_output(ls)
! OUTPUT LS MODEL RESULTS
USE GEN_UTIL, ONLY : time2str
USE GEN_FILE, ONLY : openfile
USE GEN_STRING, ONLY : pad, num2str, strtrim
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(LSModTyp),INTENT(INOUT) :: ls
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'ls_output'
INTEGER(KIND=ISGL) :: i

DO i = 1,ls%Nout
    CALL update_output(ls%out(i))
    CALL do_output(ls%out(i))
END DO

CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE update_output(out)
! UPDATE OUTPUT DATA
TYPE(GenOutTyp),INTENT(INOUT) :: out
! LOCAL VARIABLES

SELECT CASE(out%typ)
CASE(3) ! TRANSPORT CSV
    CALL update_transport_output(out)
END SELECT

END SUBROUTINE update_output
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE update_transport_output(out)
TYPE(GenOutTyp),INTENT(INOUT) :: out
! LOCAL VARIABLES

out%dat(1)%x = out%dat(1)%x + 1.
out%dat(2)%x = out%dat(2)%x + LS%Qs
out%dat(3)%x = out%dat(3)%x + LS%Qspot

END SUBROUTINE update_transport_output
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE do_output(out)
! PROCESS A SINGLE OUTPUT REQUEST
TYPE(GenOutTyp),INTENT(INOUT) :: out
! LOCAL VARIABLES

IF (out%t > t+0.0001_DBL .OR. out%t > out%tfinal) RETURN

SELECT CASE(out%typ)
CASE(1) ! SHORELINE CSV
    CALL message('    Writing shoreline csv output.','(a\)')
    CALL do_shoreline_output(out)
CASE(2) ! LONGSHORE CSV
    CALL message('    Writing longshore csv output.','(a\)')
    CALL do_longshore_output(out)
CASE(3) ! TRANSPORT CSV
    CALL message('    Writing transport csv output.','(a\)')
    CALL do_transport_output(out)
END SELECT

IF (tform==0) THEN
    CALL message(' t = '//TRIM(time2str(t,tform,tzero))//' days.','(a)')
ELSE
    CALL message(' t = '//TRIM(time2str(t,tform,tzero))//'.','(a)')
END IF

out%t = MAX(out%t,t) + REAL(out%dt,KIND=DBL)

END SUBROUTINE do_output
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE do_shoreline_output(out)
TYPE(GenOutTyp),INTENT(INOUT) :: out
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: i, j, ierr
REAL(KIND=SGL) :: chain, eta
REAL(KIND=DBL),DIMENSION(2) :: base
REAL(KIND=DBL),DIMENSION(2,LS%Ny-1) :: coords

IF (.NOT.out%init) THEN
    CALL openfile(UNIT=out%fid,FILE=out%fil,STATUS=pad('REPLACE',15),&
                ACCESS=pad('SEQUENTIAL',15),ACTION=pad('READWRITE',15),&
                FORM=pad('FORMATTED',15))
    CALL errchk(sub); IF (errstat) RETURN
    WRITE(out%fid, '(a\)',IOSTAT=ierr) 'VARIABLE,TIME/CHAINAGE'
    DO j = 1,LS%Ny-1
        chain = 0.5 * (LS%chain(j) + LS%chain(j+1))
        WRITE(out%fid, '(a\)',IOSTAT=ierr) ','//strtrim(num2str(chain))
    END DO
    WRITE(out%fid, '(a)',IOSTAT=ierr)
    out%init = .TRUE.
END IF
DO j = 1,LS%Ny-1
    base = 0.5 * (LS%base(:,j) + LS%base(:,j+1))
    eta = 0.5 * (LS%eta(j) + LS%eta(j+1))
    coords(:,j) = calc_coords(base,eta,LS%x0(j) )
END DO

WRITE(out%fid,'(a\)',IOSTAT=ierr) 'X'
WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(time2str(t,tform,tzero))
DO j = 1,LS%Ny-1 
    WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(num2str(coords(1,j)))
END DO
WRITE(out%fid,'(a)',IOSTAT=ierr)

WRITE(out%fid,'(a\)',IOSTAT=ierr) 'Y'
WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(time2str(t,tform,tzero))
DO j = 1,LS%Ny-1 
    WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(num2str(coords(2,j)))
END DO
WRITE(out%fid,'(a)',IOSTAT=ierr)

END SUBROUTINE do_shoreline_output
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE do_longshore_output(out)
TYPE(GenOutTyp),INTENT(INOUT) :: out
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: i, j, ierr, n
REAL(KIND=SGL) :: chain, eta
REAL(KIND=DBL),DIMENSION(2) :: base
REAL(KIND=DBL),DIMENSION(2,LS%Ny-1) :: coords

IF (.NOT.out%init) THEN
    CALL openfile(UNIT=out%fid,FILE=out%fil,STATUS=pad('REPLACE',15),&
                ACCESS=pad('SEQUENTIAL',15),ACTION=pad('READWRITE',15),&
                FORM=pad('FORMATTED',15))
    CALL errchk(sub); IF (errstat) RETURN
    WRITE(out%fid, '(a\)',IOSTAT=ierr) 'VARIABLE,TIME/CHAINAGE'
    DO j = 1,LS%Ny-1
        chain = 0.5 * (LS%chain(j) + LS%chain(j+1))
        WRITE(out%fid, '(a\)',IOSTAT=ierr) ','//strtrim(num2str(chain))
    END DO
    WRITE(out%fid, '(a)',IOSTAT=ierr)
    out%init = .TRUE.
END IF

WRITE(out%fid,'(a\)',IOSTAT=ierr) 'X0'
WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(time2str(t,tform,tzero))
DO j = 1,LS%Ny-1 
    WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(num2str(ls%x0(j)))
END DO
WRITE(out%fid,'(a)',IOSTAT=ierr)

DO n = 1,ls%NL
    WRITE(out%fid,'(a\)',IOSTAT=ierr) 'XA'//num2str(n-1)
    WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(time2str(t,tform,tzero))
    DO j = 1,LS%Ny-1 
        WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(num2str(ls%xl(n,j)))
    END DO
    WRITE(out%fid,'(a)',IOSTAT=ierr)
END DO

END SUBROUTINE do_longshore_output
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE do_transport_output(out)
TYPE(GenOutTyp),INTENT(INOUT) :: out
! LOCAL PARAMETERS
REAL(KIND=SGL) :: convert_transport = 365.25 * 24. * 3600.
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: i, j, ierr
REAL(KIND=SGL) :: thetab

IF (.NOT.out%init) THEN
    CALL openfile(UNIT=out%fid,FILE=out%fil,STATUS=pad('REPLACE',15),&
                ACCESS=pad('SEQUENTIAL',15),ACTION=pad('READWRITE',15),&
                FORM=pad('FORMATTED',15))
    CALL errchk(sub); IF (errstat) RETURN
    WRITE(out%fid, '(a\)') 'VARIABLE,TIME/CHAINAGE'
    DO j = 1,LS%Ny
        WRITE(out%fid, '(a\)') ','//strtrim(num2str(LS%chain(j)))
    END DO
    WRITE(out%fid, '(a)')
    out%init = .TRUE.
END IF

WRITE(out%fid,'(a\)',IOSTAT=ierr) 'thetab'
WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(time2str(t,tform,tzero))
DO j = 1,LS%Ny
    thetab = LS%thetab(j) + LS%eta(j)
    WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(num2str(thetab*rad_to_deg))
END DO
WRITE(out%fid,'(a)',IOSTAT=ierr)

WRITE(out%fid,'(a\)',IOSTAT=ierr) 'alphab'
WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(time2str(t,tform,tzero))
DO j = 1,LS%Ny
    WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(num2str(LS%alphab(j)*rad_to_deg))
END DO
WRITE(out%fid,'(a)',IOSTAT=ierr)

out%dat(3)%x = out%dat(3)%x / out%dat(1)%x(1)
WRITE(out%fid,'(a\)',IOSTAT=ierr) 'Qspot'
WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(time2str(t,tform,tzero))
DO j = 1,LS%Ny
    WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(num2str(out%dat(3)%x(j)*convert_transport))
END DO
WRITE(out%fid,'(a)',IOSTAT=ierr)

out%dat(2)%x = out%dat(2)%x / out%dat(1)%x(1)
WRITE(out%fid,'(a\)',IOSTAT=ierr) 'Qs'
WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(time2str(t,tform,tzero))
DO j = 1,LS%Ny
    WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(num2str(out%dat(2)%x(j)*convert_transport))
END DO
WRITE(out%fid,'(a)',IOSTAT=ierr)

out%dat(1)%x = 0.
out%dat(2)%x = 0.
out%dat(3)%x = 0.

END SUBROUTINE do_transport_output
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE ls_output
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!


END MODULE LSmod
