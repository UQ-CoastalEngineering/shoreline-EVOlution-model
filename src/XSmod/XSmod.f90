! XSHORE SHORELINE EVOLUTION MODULE

! PROFILE TYPE 0 = 'SIMPLE'
! par(1) => dune height above WL
! par(2) => dune slope
! par(3) => active profile height
! par(4) => active profile slope
! par(5) => offshore profile slope
! xvar(1) => apparent shoreline location (equivalent to shoreline in absence of seawall)

! PROFILE TYPE 1 = 'HUXLEY'
! par(1) => dune elevation
! par(2) => dune slope
! par(3) => surf zone power function "A" parameter
! par(4) => transition slope
! par(5) => offshore slope
! par(6) => offshore elevation
! par(7) => Ke parameter (erosion rate)
! par(8) => Ka parameter (accretion rate)
! xvar(1),zvar(1) => apparent shoreline location
! xvar(2),zvar(2) => apparent bar location

! SEAWALL
! wall(1) => z top
! wall(2) => x seaward


MODULE XSmod

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
PUBLIC :: XSmodtyp, construct, destruct, initialise, update, output

! MODULE INTERFACES
INTERFACE construct
    MODULE PROCEDURE xs_construct
END INTERFACE
INTERFACE destruct
    MODULE PROCEDURE xs_destruct
END INTERFACE
INTERFACE ASSIGNMENT (=)
    MODULE PROCEDURE copy_profile_vars
END INTERFACE
INTERFACE initialise
    MODULE PROCEDURE xs_initialise
END INTERFACE
INTERFACE update
    MODULE PROCEDURE xs_update_position
    MODULE PROCEDURE xs_update_forcing
END INTERFACE
INTERFACE output
    MODULE PROCEDURE xs_output
END INTERFACE

! MODULE TYPES
! Profile type
TYPE XSproftyp
!    PRIVATE
    INTEGER(KIND=ISGL) :: typ                                   ! Profile type
    CHARACTER(LEN=SSTR) :: name                                 ! Profile name
    INTEGER(KIND=ISGL) :: Npar                                  ! Number of profile parameters
    REAL(KIND=DBL),ALLOCATABLE,DIMENSION(:) :: par              ! Profile parameters [Npar]
    INTEGER(KIND=ISGL) :: Nvar                                  ! Number of profile position variables
    REAL(KIND=DBL),ALLOCATABLE,DIMENSION(:) :: xvar             ! Profile x position variables [Nvar]
    REAL(KIND=DBL),ALLOCATABLE,DIMENSION(:) :: zvar             ! Profile z position variables [Nvar]
    REAL(KIND=DBL),DIMENSION(2) :: dy                           ! Profile start/end width (m)
    REAL(KIND=DBL) :: length                                    ! Profile length (m)
    REAL(KIND=DBL) :: vol_upper_per_m                           ! Profile integrated volume per unit beach width above 0.0mRL (m^2)
    REAL(KIND=DBL) :: vol_per_m                                 ! Profile integrated volume per unit beach width (m^2)
    REAL(KIND=DBL) :: vol                                       ! Profile integrated volume (m^3)
    REAL(KIND=DBL) :: vol_error                                 ! Profile cumulative volume error (m^3)
    REAL(KIND=DBL) :: vol_min                                   ! Profile minimum possible volume (m^3)
    INTEGER(KIND=ISGL) :: Nx                                    ! Number of points in profile
    REAL(KIND=DBL),ALLOCATABLE,DIMENSION(:) :: xa               ! Apparent profile cross-shore chainage (m) [Nx]
    REAL(KIND=DBL),ALLOCATABLE,DIMENSION(:) :: za               ! Apparent profile elevation (m) [Nx]
    REAL(KIND=DBL),ALLOCATABLE,DIMENSION(:) :: x                ! Profile cross-shore chainage (m) [Nx]
    REAL(KIND=DBL),ALLOCATABLE,DIMENSION(:) :: z                ! Profile elevation (m) [Nx]
    LOGICAL,ALLOCATABLE,DIMENSION(:) :: stat                    ! Profile longshore transport status [Nx]
    LOGICAL :: errstat
    CHARACTER(LEN=LSTR) :: errstr
END TYPE
! XS model type
TYPE :: XSmodtyp
!    PRIVATE
    INTEGER(KIND=ISGL) :: typ                                   ! Profile model type
    INTEGER(KIND=ISGL) :: Nprof                                 ! Number of profiles defined in type
    INTEGER(KIND=ISGL) :: Nvar                                  ! Number of profile position variables
    INTEGER(KIND=ISGL) :: Nout                                  ! Number of output requests
    INTEGER(KIND=ISGL) :: id                                    ! Profile id (if provided)
    TYPE(XSproftyp),ALLOCATABLE,DIMENSION(:) :: prof            ! Model profiles [Nprof]
    REAL(KIND=DBL),ALLOCATABLE,DIMENSION(:) :: wall             ! Seawall (if defined)
    REAL(KIND=DBL) :: wl                                        ! Current water level
    TYPE(WvParTyp) :: wv                                        ! Breaking wave parameters
    REAL(KIND=DBL),ALLOCATABLE,DIMENSION(:) :: xvar             ! Current profile x position variables [Nvar]
    REAL(KIND=DBL),ALLOCATABLE,DIMENSION(:) :: zvar             ! Current profile z position variables [Nvar]
    REAL(KIND=DBL) :: x0                                        ! Current shoreline position
    REAL(KIND=DBL) :: x0a                                       ! Apparent shoreline position
    REAL(KIND=DBL) :: xbr                                       ! Current breaker position
    TYPE(GenOutTyp),ALLOCATABLE,DIMENSION(:) :: out             ! Xs model output objects [Nout]
END TYPE

! MODULE PARAMETERS
INTEGER(KIND=ISGL),PARAMETER :: Nprofmax = 10

! MODULE VARIABLES

! MODULE PROCEDURES
CONTAINS
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE xs_construct(xs,typ,par,length,id,dy,wall,output)
! XS object constructor routine (public)
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(XSmodtyp),INTENT(OUT) :: xs
INTEGER(KIND=ISGL),INTENT(IN) :: typ
REAL(KIND=SGL),DIMENSION(:),INTENT(IN) :: par
REAL(KIND=SGL),INTENT(IN) :: length
INTEGER(KIND=ISGL),OPTIONAL,INTENT(IN) :: id
REAL(KIND=SGL),DIMENSION(2),OPTIONAL,INTENT(IN) :: dy
REAL(KIND=SGL),DIMENSION(:),OPTIONAL,INTENT(IN) :: wall
CLASS(OutCtrlTyp),DIMENSION(:),OPTIONAL,INTENT(IN) :: output
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'xs_construct'
INTEGER(KIND=ISGL) :: i, Nprof, Nsect, Npar, Nvar, Nl, Nx
INTEGER(KIND=ISGL) :: alloc_stat, tmpstat
CHARACTER(LEN=MSTR),DIMENSION(Nprofmax) :: profname

SELECT CASE (typ)
CASE (0) ! Simple
    Nprof = 2 ! 2 profiles, 'current' & 'old'
    profname(1) = 'current'
    profname(2) = 'old'
    Npar = 9 ! 9 parameters
    Nvar = 1 ! 1 variables
    Nx = 8 ! 8 points across profile (including seawalls)
CASE (1) ! Huxley
    Nprof = 3 ! 3 profiles, 'current', 'equilibrium' & 'old'
    profname(1) = 'current'
    profname(2) = 'equilibrium'
    profname(3) = 'old'
    Npar = 9 ! 9 parameters
    Nvar = 2 ! 2 variables
    Nx = 30 ! 30 points across profile
CASE DEFAULT
    CALL err(sub,'unknown profile type')
END SELECT

! Check input sizes
IF (SIZE(par)/=Npar) THEN
    CALL err(sub,'incorrect input argument par size'); RETURN
END IF
IF (PRESENT(wall)) THEN
    IF (SIZE(wall)/=2) THEN
        CALL err(sub,'incorrect input argument wall size'); RETURN
    END IF
END IF

! Allocate component memory
alloc_stat = 0
ALLOCATE(xs%prof(Nprof), STAT=tmpstat); IF (tmpstat/=0) alloc_stat=tmpstat
DO i = 1,Nprof
    ALLOCATE(xs%prof(i)%par(Npar), STAT=tmpstat); IF (tmpstat/=0) alloc_stat=tmpstat
    ALLOCATE(xs%prof(i)%xvar(Nvar), STAT=tmpstat); IF (tmpstat/=0) alloc_stat=tmpstat
    ALLOCATE(xs%prof(i)%zvar(Nvar), STAT=tmpstat); IF (tmpstat/=0) alloc_stat=tmpstat
    ALLOCATE(xs%prof(i)%xa(Nx), STAT=tmpstat); IF (tmpstat/=0) alloc_stat=tmpstat
    ALLOCATE(xs%prof(i)%za(Nx), STAT=tmpstat); IF (tmpstat/=0) alloc_stat=tmpstat
    ALLOCATE(xs%prof(i)%x(Nx), STAT=tmpstat); IF (tmpstat/=0) alloc_stat=tmpstat
    ALLOCATE(xs%prof(i)%z(Nx), STAT=tmpstat); IF (tmpstat/=0) alloc_stat=tmpstat
    ALLOCATE(xs%prof(i)%stat(Nx), STAT=tmpstat); IF (tmpstat/=0) alloc_stat=tmpstat
END DO
ALLOCATE(xs%xvar(Nvar), STAT=tmpstat); IF (tmpstat/=0) alloc_stat=tmpstat
ALLOCATE(xs%zvar(Nvar), STAT=tmpstat); IF (tmpstat/=0) alloc_stat=tmpstat
IF (PRESENT(wall)) THEN
    ALLOCATE(xs%wall(2), STAT=tmpstat); IF (tmpstat/=0) alloc_stat=tmpstat
END IF
IF (alloc_stat/=0) THEN
    CALL err(sub,'Allocate error'); RETURN
END IF
! Initialise component values
xs%typ = typ
xs%Nprof = Nprof
xs%Nvar = Nvar
IF (PRESENT(id)) THEN
    xs%id = id
ELSE
    xs%id = 0
END IF
DO i = 1,Nprof
    xs%prof(i)%typ = typ
    xs%prof(i)%name = profname(i)
    xs%prof(i)%par = Npar
    xs%prof(i)%par = par
    xs%prof(i)%length = length
    xs%prof(i)%Nvar = Nvar
    xs%prof(i)%xvar = 0._DBL
    xs%prof(i)%zvar = 0._DBL
    xs%prof(i)%vol_upper_per_m = 0._DBL
    xs%prof(i)%vol_per_m = 0._DBL
    xs%prof(i)%vol = 0._DBL
    xs%prof(i)%vol_error = 0._DBL
    IF (PRESENT(dy)) THEN
        xs%prof(i)%dy = dy
    ELSE
        xs%prof(i)%dy = 1. ! Default unit width
    END IF
    xs%prof(i)%Nx = Nx
    xs%prof(i)%xa = 0._DBL
    xs%prof(i)%za = 0._DBL
    xs%prof(i)%x = 0._DBL
    xs%prof(i)%z = 0._DBL
    xs%prof(i)%stat = 0._DBL
    xs%prof(i)%errstat = .FALSE.
    xs%prof(i)%errstr = ''
END DO
xs%wl = 0._DBL
xs%xvar = 0._DBL
xs%zvar = 0._DBL
xs%x0 = 0._DBL
xs%x0a = 0._DBL
xs%xbr = 0._DBL
IF (PRESENT(wall)) THEN
    xs%wall = wall
END IF

CALL construct_output()
CALL errchk(sub); IF (errstat) RETURN

CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE construct_output()
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: Ntmp, Nout, i

IF (.NOT.PRESENT(output)) RETURN
Ntmp = SIZE(output,1)
IF (Ntmp==0) RETURN
! Count XS model related outputs
Nout = 0
DO i = 1,Ntmp
    SELECT CASE (output(i)%typnam)
    CASE('XSECT','EQUILIBRIUMXSECT')
        Nout = Nout + 1
    END SELECT
END DO
IF (Nout==0) RETURN

! Allocate wv%out
alloc_stat = 0
ALLOCATE(xs%out(Nout),STAT=alloc_stat)
IF (alloc_stat/=0) THEN
    CALL err(sub, 'Allocate error.')
    RETURN
END IF
! Initialise values
xs%Nout = Nout
Nout = 0
DO i = 1,Ntmp
    SELECT CASE (output(i)%typnam)
    CASE('XSECT')
        Nout = Nout + 1
        xs%out(Nout)%typ = 1
    CASE('EQUILIBRIUMXSECT')
        Nout = Nout + 1
        xs%out(Nout)%typ = 2
    CASE DEFAULT
        CYCLE
    END SELECT
    SELECT TYPE (out=>output(i))
    TYPE IS (OutCtrlTyp)
        xs%out(Nout)%OutCtrlTyp = out
        xs%out(Nout)%fid = 0
        xs%out(Nout)%init = .FALSE.
        xs%out(Nout)%t = out%tstart
    TYPE IS (GenOutTyp)
        xs%out(Nout) = out
    END SELECT
END DO

END SUBROUTINE construct_output
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE xs_construct
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE xs_destruct(xs)
! XS object destructor routine (public)
USE GEN_FILE, ONLY : closefile
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(XSmodtyp),INTENT(INOUT) :: xs
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'xs_destruct'
INTEGER(KIND=ISGL) :: i, j
INTEGER(KIND=ISGL) :: dealloc_stat, tmpstat, ierr

DO i = 1,SIZE(xs%out)
    CALL closefile(UNIT=xs%out(i)%fid,IOSTAT=ierr)
END DO
DEALLOCATE(xs%zvar, STAT=tmpstat); IF (tmpstat/=0) dealloc_stat=tmpstat
DEALLOCATE(xs%xvar, STAT=tmpstat); IF (tmpstat/=0) dealloc_stat=tmpstat
DEALLOCATE(xs%wall, STAT=tmpstat); IF (tmpstat/=0) dealloc_stat=tmpstat
DO i = 1,SIZE(xs%prof)
    DEALLOCATE(xs%prof(i)%stat, STAT=tmpstat); IF (tmpstat/=0) dealloc_stat=tmpstat
    DEALLOCATE(xs%prof(i)%z, STAT=tmpstat); IF (tmpstat/=0) dealloc_stat=tmpstat
    DEALLOCATE(xs%prof(i)%x, STAT=tmpstat); IF (tmpstat/=0) dealloc_stat=tmpstat
    DEALLOCATE(xs%prof(i)%za, STAT=tmpstat); IF (tmpstat/=0) dealloc_stat=tmpstat
    DEALLOCATE(xs%prof(i)%xa, STAT=tmpstat); IF (tmpstat/=0) dealloc_stat=tmpstat
    DEALLOCATE(xs%prof(i)%zvar, STAT=tmpstat); IF (tmpstat/=0) dealloc_stat=tmpstat
    DEALLOCATE(xs%prof(i)%xvar, STAT=tmpstat); IF (tmpstat/=0) dealloc_stat=tmpstat
    DEALLOCATE(xs%prof(i)%par, STAT=tmpstat); IF (tmpstat/=0) dealloc_stat=tmpstat
END DO
DEALLOCATE(xs%prof, STAT=tmpstat); IF (tmpstat/=0) dealloc_stat=tmpstat
xs%typ =0
xs%Nprof = 0
xs%Nvar = 0
xs%Nout = 0

END SUBROUTINE xs_destruct
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE copy_profile_vars(prof2,prof1)
! Copy profile variable fields routine
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(XSproftyp),INTENT(INOUT) :: prof2
TYPE(XSproftyp),INTENT(IN) :: prof1
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'copy_profile_vars'
LOGICAL :: error

! Check type and size compatability
error = .FALSE.
IF (prof1%typ/=prof2%typ) error = .TRUE.
IF (prof1%Nvar/=prof2%Nvar) error = .TRUE.
IF (prof1%Nx/=prof2%Nx) error = .TRUE.
IF (error) THEN
    CALL err(sub,'profile objects are not compatible for copying'); RETURN
END IF

prof2%xvar = prof1%xvar
prof2%zvar = prof1%zvar
prof2%vol_per_m = prof1%vol_per_m
prof2%vol = prof1%vol
prof2%vol_error = prof1%vol_error
prof2%xa = prof1%xa
prof2%za = prof1%za
prof2%x = prof1%x
prof2%z = prof1%z
prof2%stat = prof1%stat

END SUBROUTINE copy_profile_vars
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE build_profile(prof,wl,wv)
! Build profile z elevations
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(XSproftyp),INTENT(INOUT) :: prof
REAL(KIND=DBL),INTENT(IN) :: wl
TYPE(WvParTyp),INTENT(IN) :: wv

SELECT CASE (prof%typ)
CASE (0)
    CALL build_profile_0(prof,wl,wv)
CASE (1)
    CALL build_profile_1(prof,wl,wv)
END SELECT

CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE build_profile_0(prof,wl,wv)
! Build simple profile
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(XSproftyp),INTENT(INOUT) :: prof
REAL(KIND=DBL),INTENT(IN) :: wl
TYPE(WvParTyp),INTENT(IN) :: wv
! LOCAL PARAMETERS
REAL(KIND=DBL),PARAMETER :: small = 0.001_DBL
! LOCAL VARIABLES
REAL(KIND=DBL) :: x0, z0, xd, zd, h1, m0, m1, m2, m3, xoff, zoff
REAL(KIND=DBL) :: b0, b2, b3, x1, z1, x2, z2

prof%errstat = .FALSE.

x0 = prof%xvar(1)
z0 = prof%zvar(1)

zd = prof%par(1)
m0 = -prof%par(2)
h1 = prof%par(4)
m1 = -prof%par(5)
m2 = -prof%par(6)
m3 = -prof%par(7)
zoff = prof%par(8)
xoff = prof%length

b0 = z0 - m0 * x0
xd = (zd - b0) / m0
x1 = x0 - h1 / m1
z1 = z0 - h1
b2 = z1 - m2 * x1
b3 = zoff - m3 * xoff
x2 = (b3 - b2) / (m2 - m3)
z2 = m3 * x2 + b3
!IF (x2>x1) THEN
!    z2 = m3 * x2 + b3
!ELSE
!    z2 = z1
!    x2 = (z2 - b3) / m3
!END IF

IF (x2<x1) THEN
    prof%errstat = .TRUE.
    prof%errstr = 'Non-intersecting upper and lower profile'
END IF

prof%xa(1) = 0._DBL
prof%za(1) = zd
prof%xa(2) = xd
prof%za(2) = zd
prof%xa(3) = x0
prof%za(3) = z0
prof%xa(4) = x1
prof%za(4)= z1
prof%xa(5) = x2
prof%za(5) = z2
prof%xa(6) = xoff
prof%za(6) = zoff
prof%xa(7:) = prof%xa(6)
prof%za(7:) = prof%za(6)
prof%stat = .TRUE.

END SUBROUTINE build_profile_0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE build_profile_1(prof,wl,wv)
! Build Huxley profile
! USE GEN_UTIL, ONLY : arth
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(XSproftyp),INTENT(INOUT) :: prof
REAL(KIND=DBL),INTENT(IN) :: wl
TYPE(WvParTyp),INTENT(IN) :: wv
! LOCAL PARAMETERS
INTEGER(KIND=ISGL) :: i
INTEGER(KIND=ISGL),PARAMETER :: NA = 20
REAL(KIND=DBL),PARAMETER :: d2_3rds = 2._DBL / 3._DBL
REAL(KIND=DBL),PARAMETER :: small = 0.001_DBL
! LOCAL VARIABLES
REAL(KIND=DBL) :: x0, z0, xb, zb
REAL(KIND=DBL) :: zd, m0, A, m1, m2, zoff, xoff
REAL(KIND=DBL) :: b0, xd, dx, b1, b2, xt, zt
REAL(KIND=DBL) :: arth(20) = (/(i, i=0,19, 1)/)

prof%errstat = .FALSE.

x0 = prof%xvar(1)
z0 = prof%zvar(1)
xb = prof%xvar(2)
zb = prof%zvar(2)

zd = prof%par(1)
m0 = -prof%par(2)
m1 = -prof%par(4)
m2 = -prof%par(5)
zoff = prof%par(6)
xoff = prof%length

b0 = z0 - m0 * x0
xd = (zd - b0) / m0
A = (z0 - zb) / (xb - x0)**d2_3rds
dx = (xb - x0) / REAL(NA, KIND=DBL)
b1 = zb - m1 * xb
b2 = zoff - m2 * xoff
xt = (b2 - b1) / (m1 - m2)
zt = m2 * xt + b2

!IF (xt>xb) THEN
!    zt = m2 * xt + b2
!ELSE
!    zt = zb
!    xt = (zt - b2) / m2
!END IF

IF (xt<xb) THEN
    prof%errstat = .TRUE.
    prof%errstr = 'Non-intersecting upper and lower profile'
END IF

prof%xa(1) = 0._DBL
prof%za(1) = zd
prof%xa(2) = xd
prof%za(2) = zd
prof%xa(3:NA+2) = arth*dx+x0
prof%za(3:NA+2)= z0 - A * (prof%xa(3:NA+2) - x0) ** d2_3rds
prof%xa(NA+3) = xt
prof%za(NA+3) = zt
prof%xa(NA+4:) = xoff
prof%za(NA+4:) = zoff
prof%stat = .TRUE.

END SUBROUTINE build_profile_1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE build_profile
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE build_seawall(prof,wall)
! Add seawall to profile (if specified)
USE GEN_UTIL, ONLY : interp1
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(XSproftyp),INTENT(INOUT) :: prof
REAL(KIND=DBL),ALLOCATABLE,DIMENSION(:),INTENT(IN) :: wall
! LOCAL PARAMETERS
REAL(KIND=DBL) :: small = 1.0e-5_DBL
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: Nx, i
REAL(KIND=DBL) :: zwall, xwall, zi

IF (.NOT.ALLOCATED(wall) .OR. wall(2)<small) THEN
    prof%x = prof%xa
    prof%z = prof%za
    RETURN
END IF

Nx = prof%Nx
zwall = wall(1)
xwall = wall(2)

i = maxloc(prof%xa-xwall,MASK=prof%xa-xwall .LE. 0,DIM=1) 
zi = interp1(prof%xa,prof%za,xwall)

prof%x(1:i) = prof%xa(1:i)
prof%z(1:i) = MAX(prof%za(1:i),zwall)
prof%x(i+3:Nx) = prof%xa(i+1:Nx-2) ! insert 2 points in profile for seawall
prof%z(i+3:Nx) = prof%za(i+1:Nx-2)
prof%x(i+1) = xwall
prof%z(i+1) = MAX(zi,zwall)
prof%x(i+2) = xwall + small
prof%z(i+2) = zi
prof%stat(1:i+1) = prof%z(1:i+1)>zwall+small

END SUBROUTINE build_seawall
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE calc_vol(prof)
! Calculate profile volume
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(XSproftyp),INTENT(INOUT) :: prof
! LOCAL PARAMETERS
!REAL(KIND=DBL) :: dx_min = 100000._DBL
REAL(KIND=DBL),PARAMETER :: zup0 = 0._DBL
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: i, Nx
REAL(KIND=DBL) :: Vol_upper_per_m, Vol_per_m, Vol
REAL(KIND=DBL) :: dx_min, dx, dydx, xold, xnew, dyold, dynew, zref, zup, zold, znew, dVold, dVnew
REAL(KIND=DBL) :: z1, z2, delt
LOGICAL :: stat

Vol_upper_per_m = 0._DBL
Vol_per_m = 0._DBL
Vol = 0._DBL

Nx = prof%Nx
zref = prof%z(Nx)
zup = zup0 - zref
dydx = (prof%dy(2)-prof%dy(1)) / (prof%x(Nx)-prof%x(1))
!dx_min = 0.1_DBL / MAX(ABS(dydx), 1.0e-5_DBL)
!dx_min = MAX(dx_min,10._DBL)
dx_min = 100._DBL
xold = prof%x(1)    
zold = prof%z(1) - prof%z(Nx)
dyold = prof%dy(1)
dVold = dyold * zold
stat = prof%stat(1)
i = 2
DO WHILE (i<=Nx)
    xnew = xold + dx_min
    IF (xnew < prof%x(i)) THEN
        znew = prof%z(i-1) + (xnew - prof%x(i-1)) / (prof%x(i) - prof%x(i-1)) * (prof%z(i) - prof%z(i-1))
        znew = znew - zref
    ELSE
        xnew = prof%x(i)
        znew = prof%z(i) - zref
        stat = prof%stat(i)
        i = i + 1
    END IF
    dx = xnew - xold
    dynew = dyold + dydx * dx
    dVnew = dynew * znew
    IF (zold > zup) THEN
        IF (znew > zup) THEN
            delt = dx; z1 = zold; z2 = znew
        ELSE
            delt = (zup - zold) / (znew - zold) * dx
            z1 = zold; z2 = zup
        END IF
    ELSE
        IF (zold > zup) THEN
            delt = (zup - znew) / (zold - znew) * dx
            z1 = zup; z2 = znew
        ELSE
            delt = 0._DBL; z1 = 0._DBL; z2 = 0._DBL
        END IF
    END IF
    Vol_upper_per_m = Vol_upper_per_m + 0.5_DBL * (z1 + z2 - 2._DBL * zup) * delt
!    IF (stat) THEN
        Vol_per_m = Vol_per_m + 0.5_DBL * (zold + znew) * dx
        Vol = Vol + 0.5_DBL * (dVold + dVnew) * dx
!    END IF
    xold = xnew; zold = znew; dyold = dynew; dVold = dVnew
END DO

prof%Vol_upper_per_m = Vol_upper_per_m
prof%Vol_per_m = Vol_per_m
prof%Vol = Vol

END SUBROUTINE calc_vol
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE xs_initialise(xs,xinit,zinit,wl,wv)
! Initialise XS object for specified variables (including optional seawall)
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(XSmodtyp),INTENT(INOUT) :: xs
REAL(KIND=SGL),DIMENSION(:),INTENT(IN) :: xinit
REAL(KIND=SGL),DIMENSION(:),INTENT(IN) :: zinit
REAL(KIND=SGL),INTENT(IN) :: wl
TYPE(WvParTyp),INTENT(IN) :: wv
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'xs_initialise'
INTEGER(KIND=ISGL) :: i

IF (SIZE(xinit)/=xs%Nvar) THEN
    CALL err(sub,'xinit argument is not size compatible with XS object')
    RETURN
END IF
! Set profile(1) variables
xs%prof(1)%xvar = xinit
xs%prof(1)%zvar = zinit
! Update forcing components
CALL xs_update_forcing(xs,wl,wv)
! Build profile
CALL build_profile(xs%prof(1),xs%wl,xs%wv)
IF (xs%prof(1)%errstat) THEN
    CALL err(sub,xs%prof(1)%errstr)
    RETURN
END IF
CALL build_seawall(xs%prof(1),xs%wall)
! Calculate volume
CALL calc_vol(xs%prof(1))
! Copy to all profile components
DO i = 2,xs%Nprof
    xs%prof(i) = xs%prof(1)
END DO
xs%xvar = xs%prof(1)%xvar
xs%zvar = xs%prof(1)%zvar

END SUBROUTINE xs_initialise
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE calc_new_profile(profnew,profold,dVol,wl,wv,wall)
! Calculate new profile by minimising volume change error
!USE GEN_UTIL, ONLY : fzero, zbrac
USE GEN_UTIL, ONLY : fzero
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(XSproftyp),INTENT(INOUT) :: profnew
TYPE(XSproftyp),INTENT(IN) :: profold
REAL(KIND=DBL),INTENT(IN) :: dVol
REAL(KIND=DBL),INTENT(IN) :: wl
TYPE(WvParTyp),INTENT(IN) :: wv
REAL(KIND=DBL),ALLOCATABLE,DIMENSION(:),INTENT(IN) :: wall
! LOCAL PARAMETERS
REAL(KIND=DBL) :: small = EPSILON(0._DBL)
REAL(KIND=DBL) :: width = 1.0_DBL
REAL(KIND=DBL) :: precision = 0.000001_DBL
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'calc_new_profile'
INTEGER(KIND=ISGL) :: stat
REAL(KIND=DBL) :: dx, dx_min, dx_max
REAL(KIND=DBL) :: dVol_tmp, error_tmp, print_error
REAL(KIND=DBL),DIMENSION(profnew%Nvar) :: xvar0
LOGICAL :: success

xvar0 = profnew%xvar
dx_min = -width*20
dx_max = width*20

CALL fzero(vol_error,dx_min,dx_max,dx_min,0._DBL,precision,stat)

! Build new profile
profnew%xvar = xvar0 + dx
CALL build_profile(profnew,wl,wv)
IF (profnew%errstat) THEN
    CALL err(sub,profnew%errstr)
    RETURN
END IF
CALL build_seawall(profnew,wall)
! Calculate new volume
CALL calc_vol(profnew)
! Cumulative volume error
profnew%vol_error = profnew%vol_error + profnew%vol - profold%vol - dVol
IF (stat==-1) THEN
    CALL err(sub,'Could not bracket required position')
ELSE IF (stat==-2) THEN
    CALL warn('Desired tolerance not achieved in position update calculations')
END IF
IF (ANY(profnew%xvar<0._DBL) .OR. ANY(profnew%xvar>profnew%length)) THEN
    CALL err(sub,'Updated profile position is beyond grid limits')
    RETURN
END IF

CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
FUNCTION vol_error(dx) RESULT(error)
IMPLICIT NONE
REAL(KIND=DBL),INTENT(IN) :: dx
REAL(KIND=DBL) :: error
REAL(KIND=DBL) :: dVol_
!
profnew%xvar = xvar0 + dx
! Build new profile
CALL build_profile(profnew,wl,wv)
CALL build_seawall(profnew,wall)
! Calculate new volume
CALL calc_vol(profnew)
dVol_ = profnew%vol - profold%vol
! Volume error
error = dVol_-dVol
print_error = error
!
END FUNCTION vol_error
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE calc_new_profile
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
PURE FUNCTION implicit_response_update(yold,yeq,k,dt) RESULT(ynew)
IMPLICIT NONE
! FUNCTION ARGUMENTS
REAL(KIND=DBL),INTENT(IN) :: yold
REAL(KIND=DBL),INTENT(IN) :: yeq
REAL(KIND=DBL),INTENT(IN) :: k
REAL(KIND=DBL),INTENT(IN) :: dt
! FUNCTION RESULT
REAL(KIND=DBL) :: ynew
! LOCAL VARIABLES
REAL(KIND=DBL) :: kdt

kdt = k * dt
ynew = (yold + kdt * yeq) / (1._DBL + kdt)

END FUNCTION implicit_response_update
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE xs_update_forcing(xs,wl,wv)
! Update xs forcing comnponents wl & wv
USE GEN_UTIL, ONLY : interp1
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(XSmodtyp),INTENT(INOUT) :: xs
REAL(KIND=SGL),INTENT(IN) :: wl
TYPE(WvParTyp),INTENT(IN) :: wv
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'xs_update_forcing'
INTEGER(KIND=ISGL) :: i
REAL(KIND=DBL) :: z0, zb

! Update forcing components
xs%wl = wl
xs%wv = wv
! Update shoreline and breaker positions
IF (xs%typ==0) THEN
    !z0 = xs%prof(1)%zvar(1)
    !zb = z0 - xs%prof(1)%par(4)
    z0 = wl + xs%prof(1)%par(3)
    zb = wl - wv%dpth
    xs%x0 = interp1(xs%prof(1)%z,xs%prof(1)%x,z0)
    xs%x0a = interp1(xs%prof(1)%za,xs%prof(1)%xa,z0)
    xs%xbr = interp1(xs%prof(1)%z,xs%prof(1)%x,zb)
ELSEIF (xs%typ==1) THEN
    z0 = wl + kappa * wv%dpth / (1._DBL - kappa)
    zb = wl - wv%dpth
    xs%x0 = interp1(xs%prof(1)%z,xs%prof(1)%x,z0)
    xs%x0a = interp1(xs%prof(1)%za,xs%prof(1)%xa,z0)
    xs%xbr = interp1(xs%prof(1)%z,xs%prof(1)%x,zb)
END IF

END SUBROUTINE xs_update_forcing
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE xs_update_position(xs,dVdt,dt_)
! Update new profile
USE GEN_UTIL, ONLY : interp1
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(XSmodtyp),INTENT(INOUT) :: xs
REAL(KIND=SGL),INTENT(IN) :: dVdt
REAL(KIND=SGL),INTENT(IN) :: dt_
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'xs_update'
REAL(KIND=DBL) :: dVol, dt

dt = REAL(dt_,KIND=DBL)
dVol = REAL(dVdt,KIND=DBL) * dt
SELECT CASE (xs%typ)
CASE (0)
    CALL xs_update_profile_0
CASE (1)
    CALL xs_update_profile_1
END SELECT

CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE xs_update_profile_0
IMPLICIT NONE
! LOCAL VARIABLES
REAL(KIND=DBL) :: k, h0, h1, hb, z0eq, z0, zb
!
! Copy new profile to old
xs%prof(2) = xs%prof(1)
! Calculate new profile (minimising volume error)
k = xs%prof(1)%par(9)
h0 = xs%prof(1)%par(3)
!h1 = xs%prof(1)%par(4)
hb = MAX(hb, 1.0)
zb = xs%wl - hb
z0eq = xs%wl + h0
z0 = implicit_response_update(xs%prof(1)%zvar(1),z0eq,k,dt)
xs%prof(1)%zvar(1) = z0
CALL calc_new_profile(xs%prof(1),xs%prof(2),dVol,xs%wl,xs%wv,xs%wall)
CALL errchk(sub); IF (errstat) RETURN
xs%xvar = xs%prof(1)%xvar
xs%zvar = xs%prof(1)%zvar
! Calculate actual and apparent shoreline and breaker position
xs%x0 = interp1(xs%prof(1)%z,xs%prof(1)%x,z0eq)
xs%x0a = interp1(xs%prof(1)%za,xs%prof(1)%xa,z0eq)
xs%xbr = interp1(xs%prof(1)%za,xs%prof(1)%xa,zb)
!
END SUBROUTINE xs_update_profile_0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE xs_update_profile_1
IMPLICIT NONE
! LOCAL VARIABLES
REAL(KIND=DBL) :: hb, zb, xb, z0, A, k
!
! Copy new profile to old
xs%prof(3) = xs%prof(1)
! Calculate equilibrium profile
A = xs%prof(2)%par(3)
hb = xs%wv%Ht / breaker_index
hb = MAX(hb, xs%prof(2)%par(9))
zb = xs%wl - hb
xb = xs%prof(2)%xvar(1) + (hb / A)**1.5
z0 = xs%wl + kappa * xs%wv%dpth / (1._DBL - kappa)
xs%prof(2)%zvar(1) = z0
xs%prof(2)%zvar(2) = zb
xs%prof(2)%xvar(2) = xb
! Calculate equilibrium profile position
CALL calc_new_profile(xs%prof(2),xs%prof(3),dVol,xs%wl,xs%wv,xs%wall)
CALL errchk(sub); IF (errstat) RETURN
! Calculate new profile
! Select change rate parameter
IF (xs%prof(2)%xvar(1) < xs%prof(1)%xvar(1)) THEN
    k = xs%prof(1)%par(7) ! Erosion
ELSE
    k = xs%prof(1)%par(8) ! Accretion
END IF
! Update z0, x0, zb, xb
xs%prof(1)%zvar(1) = implicit_response_update(xs%prof(1)%zvar(1),xs%prof(2)%zvar(1),k,dt) ! z0
xs%prof(1)%xvar(1) = implicit_response_update(xs%prof(1)%xvar(1),xs%prof(2)%xvar(1),k,dt) ! x0
xs%prof(1)%zvar(2) = implicit_response_update(xs%prof(1)%zvar(2),xs%prof(2)%zvar(2),k,dt) ! zb
xs%prof(1)%xvar(2) = implicit_response_update(xs%prof(1)%xvar(2),xs%prof(2)%xvar(2),k,dt) ! xb
! Calculate new profile position
CALL calc_new_profile(xs%prof(1),xs%prof(3),dVol,xs%wl,xs%wv,xs%wall)
CALL errchk(sub); IF (errstat) RETURN
xs%xvar = xs%prof(1)%xvar
xs%zvar = xs%prof(1)%zvar
! Calculate actual and apparent shoreline and breaker position
xs%x0 = interp1(xs%prof(1)%z,xs%prof(1)%x,z0)
xs%x0a = interp1(xs%prof(1)%za,xs%prof(1)%xa,z0)
xs%xbr = interp1(xs%prof(1)%z,xs%prof(1)%x,zb)
!
END SUBROUTINE xs_update_profile_1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE xs_update_position
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE xs_output(xs,echo)
! OUTPUT XS MODEL RESULTS
USE GEN_UTIL, ONLY : time2str
USE GEN_FILE, ONLY : openfile
USE GEN_STRING, ONLY : pad, num2str, strtrim
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(XSModTyp),INTENT(INOUT) :: xs
LOGICAL,OPTIONAL,INTENT(IN) :: echo
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'xs_output'
INTEGER(KIND=ISGL) :: i
LOGICAL :: echo_

IF (PRESENT(echo)) THEN
    echo_ = echo
ELSE
    echo_ = .TRUE.
END IF

DO i = 1,xs%Nout
    CALL do_output(xs%out(i))
END DO

CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE do_output(out)
! PROCESS A SINGLE OUTPUT REQUEST
TYPE(GenOutTyp),INTENT(INOUT) :: out
! LOCAL VARIABLES

IF (out%t > t+0.0001_DBL .OR. out%t > out%tfinal) RETURN

SELECT CASE(out%typ)
CASE(1) ! XSECT CSV
    IF (echo_) CALL message('    Writing xsect csv output.','(a\)')
    CALL do_xsect_output(out,1)
CASE(2) ! EQUIL XSECT CSV
    IF (xs%typ==1) THEN ! Only valid for Huxley model
        IF (echo_) CALL message('    Writing equilibrium xsect csv output.','(a\)')
        CALL do_xsect_output(out,2)
    END IF
END SELECT

IF (tform==0) THEN
    IF (echo_) CALL message(' t = '//TRIM(time2str(t,tform,tzero))//' days.','(a)')
ELSE
    IF (echo_) CALL message(' t = '//TRIM(time2str(t,tform,tzero))//'.','(a)')
END IF

out%t = MAX(out%t,t) + REAL(out%dt,KIND=DBL)

END SUBROUTINE do_output
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE do_xsect_output(out,prof_id)
TYPE(GenOutTyp),INTENT(INOUT) :: out
INTEGER(KIND=ISGL),INTENT(IN) :: prof_id
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: i, j, ierr

! Initialise
IF (out%fid==0) THEN
    CALL openfile(UNIT=out%fid,FILE=out%fil,STATUS=pad('REPLACE',15),&
                ACCESS=pad('SEQUENTIAL',15),ACTION=pad('READWRITE',15),&
                FORM=pad('FORMATTED',15))
    CALL errchk(sub); IF (errstat) RETURN
    WRITE(out%fid, '(a)',IOSTAT=ierr) 'XSECT_ID,VARIABLE,TIME/POINT'
    DO j = 1,xs%prof(prof_id)%Nx
        WRITE(out%fid, '(a\)') ','//strtrim(num2str(j))
    END DO
    WRITE(out%fid, '(a)')        
    out%init = .TRUE.
END IF

WRITE(out%fid,'(a\)',IOSTAT=ierr) 'XS_'//strtrim(num2str(xs%id))
WRITE(out%fid,'(","a\)',IOSTAT=ierr) 'X'
WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(time2str(t,tform,tzero))
DO j = 1,xs%prof(prof_id)%Nx
    WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(num2str(xs%prof(prof_id)%x(j)))
END DO
WRITE(out%fid,'(a)',IOSTAT=ierr)

WRITE(out%fid,'(a\)',IOSTAT=ierr) 'XS_'//strtrim(num2str(xs%id))
WRITE(out%fid,'(","a\)',IOSTAT=ierr) 'Z'
WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(time2str(t,tform,tzero))
DO j = 1,xs%prof(prof_id)%Nx
    WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(num2str(xs%prof(prof_id)%z(j)))
END DO
WRITE(out%fid,'(a)',IOSTAT=ierr)

WRITE(out%fid,'(a\)',IOSTAT=ierr) 'XS_'//strtrim(num2str(xs%id))
WRITE(out%fid,'(","a\)',IOSTAT=ierr) 'XA'
WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(time2str(t,tform,tzero))
DO j = 1,xs%prof(prof_id)%Nx
    WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(num2str(xs%prof(prof_id)%xa(j)))
END DO
WRITE(out%fid,'(a)',IOSTAT=ierr)

WRITE(out%fid,'(a\)',IOSTAT=ierr) 'XS_'//strtrim(num2str(xs%id))
WRITE(out%fid,'(","a\)',IOSTAT=ierr) 'ZA'
WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(time2str(t,tform,tzero))
DO j = 1,xs%prof(prof_id)%Nx
    WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(num2str(xs%prof(prof_id)%za(j)))
END DO
WRITE(out%fid,'(a)',IOSTAT=ierr)

END SUBROUTINE do_xsect_output
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE xs_output
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

END MODULE XSmod
