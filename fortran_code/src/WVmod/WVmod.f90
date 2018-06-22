! WAVE EVOLUTION MODULE

MODULE WVmod

! MODULE USE STATEMENTS
USE PRECISION
USE GEN_ERROR
USE LOG
USE GLOBALS
USE OUTmod, ONLY : OutCtrlTyp, GenOutTyp

! MODULE DECLARATIONS
IMPLICIT NONE

! MODULE ACCESS
PRIVATE
PUBLIC :: WvPartyp, WvModtyp, construct, destruct, update, output, average

! MODULE INTERFACES
INTERFACE construct
    MODULE PROCEDURE wv_construct
END INTERFACE
INTERFACE destruct
    MODULE PROCEDURE wv_destruct
END INTERFACE
INTERFACE update
    MODULE PROCEDURE wv_update
END INTERFACE
INTERFACE output
    MODULE PROCEDURE wv_output
END INTERFACE
INTERFACE average
    MODULE PROCEDURE wvtyp_average
END INTERFACE

! WAVE PARAMETER TYPE
TYPE WvPartyp
    REAL(KIND=SGL) :: dpth                                          ! water depth (m)
    REAL(KIND=SGL) :: theta                                         ! contour rotation (rad)
    REAL(KIND=SGL) :: Per                                           ! wave period (s)
    REAL(KIND=SGL) :: Dir                                           ! wave direction (rad. wave convention)
    REAL(KIND=SGL) :: Ht                                            ! wave height (m)
END TYPE
! WAVE TRANSFORMATION TYPE
TYPE WvTranstyp
    INTEGER(KIND=ISGL) :: NHt, Nper, Ndir
    REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:) :: Ht                   ! wave height values [NHt]
    REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:) :: Per                  ! wave period values [Nper]
    REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:) :: Dir                  ! wave direction values [Ndir]
    REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:,:,:) :: K                ! wave height transformation coefficient (-) [Ndir,Nper,NHt]
    REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:,:,:) :: D                ! wave direction transformation (rad) [Ndir,Nper,NHt]
END TYPE
! WAVE MODEL TYPE
TYPE WvModtyp
    INTEGER(KIND=ISGL) :: Ny                                        ! number of wave model points
    INTEGER(KIND=ISGL) :: Nout                                      ! number of output types
    REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:) :: chain                ! chainage (m) [Ny]
    TYPE(WvPartyp) :: Wvoff                                         ! offshore wave pars
    TYPE(WvPartyp),ALLOCATABLE,DIMENSION(:) :: Wvint                ! intermediate wave pars [Ny]
    TYPE(WvPartyp),ALLOCATABLE,DIMENSION(:) :: Wvbrk                ! breaking wave pars [Ny]
    CHARACTER(LEN=LSTR) :: transfil                                 ! wave transformation file
    TYPE(WvTranstyp),ALLOCATABLE,DIMENSION(:) :: trans              ! wave transformation type [Ny]
    LOGICAL,ALLOCATABLE,DIMENSION(:) :: struct                      ! structure logical flag [Ny]
    TYPE(GenOutTyp),ALLOCATABLE,DIMENSION(:) :: out                 ! wave model output type [Nout]
END TYPE

! MODULE VARIABLES

! MODULE PROCEDURES
CONTAINS
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE wv_construct(WV,chainage,transfil,output)
! WAVE MODEL OBJECT CONSTRUCTOR ROUTINE
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(WvModTyp),INTENT(OUT) :: WV
REAL(KIND=SGL),DIMENSION(:),INTENT(IN) :: chainage
CHARACTER(LEN=*),INTENT(IN) :: transfil
TYPE(OutCtrlTyp),DIMENSION(:),OPTIONAL,INTENT(IN) :: output
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'wv_construct'
INTEGER(KIND=ISGL) :: Ny, Nper, Ndir, NHt
INTEGER(KIND=ISGL) :: i
INTEGER(KIND=ISGL) :: alloc_stat, tmpstat

Ny = SIZE(chainage)
! Allocate Wv object memory
alloc_stat = 0
ALLOCATE(WV%chain(Ny), STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
ALLOCATE(WV%Wvint(Ny), STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
ALLOCATE(WV%Wvbrk(Ny), STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
ALLOCATE(WV%trans(Ny), STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
ALLOCATE(WV%struct(Ny), STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
IF (alloc_stat/=0) THEN
    CALL err(sub, 'Allocate error.')
    RETURN
END IF
! Initialise values
WV%Ny = Ny
WV%Nout = 0
WV%chain = chainage
WV%transfil = transfil
CALL init_wvpar(WV%Wvoff)
DO i = 1,Ny
    CALL init_wvpar(WV%Wvint(i))
    CALL init_wvpar(WV%Wvbrk(i))
END DO
! Process transformation file
CALL rd_transfil_nc(WV)
CALL errchk(sub); IF (errstat) RETURN
! Process structure (groyne) info
WV%struct = .FALSE.
! Process output
IF (PRESENT(output)) CALL construct_output()

CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE init_wvpar(wvpar)
TYPE(WvPartyp),INTENT(INOUT) :: wvpar
!
wvpar%dpth = 0.
wvpar%theta = 0.
wvpar%Per = 0.
wvpar%Dir = 0.
wvpar%Ht = 0.
!
END SUBROUTINE init_wvpar
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE construct_output()
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: Ntmp, Nout, i

Ntmp = SIZE(output,1)
IF (Ntmp==0) RETURN
! Count wave related outputs
Nout = 0
DO i = 1,Ntmp
    SELECT CASE (output(i)%typnam)
    CASE('OFFSHOREWAVES')
        Nout = Nout + 1
    CASE('INTERMEDIATEWAVES')
        Nout = Nout + 1
    CASE('BREAKINGWAVES')
        Nout = Nout + 1
    END SELECT
END DO
IF (Nout==0) RETURN

! Allocate wv%out
alloc_stat = 0
ALLOCATE(wv%out(Nout),STAT=alloc_stat)
IF (alloc_stat/=0) THEN
    CALL err(sub, 'Allocate error.')
    RETURN
END IF
! Initialise values
Wv%Nout = Nout
Nout = 0
DO i = 1,Ntmp
    SELECT CASE (output(i)%typnam)
    CASE('OFFSHOREWAVES')
        Nout = Nout + 1
        wv%out(Nout)%typ = 1
    CASE('INTERMEDIATEWAVES')
        Nout = Nout + 1
        wv%out(Nout)%typ = 2
    CASE('BREAKINGWAVES')
        Nout = Nout + 1
        wv%out(Nout)%typ = 3
    CASE DEFAULT
        CYCLE
    END SELECT
    wv%out(Nout)%OutCtrlTyp = output(i)
    wv%out(Nout)%fid = 0
    wv%out(Nout)%init = .FALSE.
    wv%out(Nout)%t = output(i)%tstart
END DO

END SUBROUTINE construct_output
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE wv_construct
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE wv_destruct(WV)
! WAVE MODEL OBJECT DESTRUCTOR ROUTINE
USE GEN_FILE, ONLY : closefile
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(WvModTyp),INTENT(INOUT) :: WV
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'wv_destruct'
INTEGER(KIND=ISGL) :: i
INTEGER(KIND=ISGL) :: dealloc_stat

DO i = 1,SIZE(WV%trans)
    DEALLOCATE(WV%trans(i)%Ht, STAT=dealloc_stat)
    DEALLOCATE(WV%trans(i)%Per, STAT=dealloc_stat)
    DEALLOCATE(WV%trans(i)%Dir, STAT=dealloc_stat)
    DEALLOCATE(WV%trans(i)%K, STAT=dealloc_stat)
    DEALLOCATE(WV%trans(i)%D, STAT=dealloc_stat)
END DO
DO i = 1,SIZE(WV%out)
    CALL closefile(UNIT=WV%out(i)%fid)
    CALL errchk(sub); IF (errstat) RETURN
END DO
DEALLOCATE(WV%out, STAT=dealloc_stat)
DEALLOCATE(WV%struct, STAT=dealloc_stat)
DEALLOCATE(WV%trans, STAT=dealloc_stat)
DEALLOCATE(WV%WVbrk, STAT=dealloc_stat)
DEALLOCATE(WV%WVint, STAT=dealloc_stat)
DEALLOCATE(WV%chain, STAT=dealloc_stat)
WV%transfil = ''
WV%Nout = 0
WV%Ny = 0

END SUBROUTINE wv_destruct
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE rd_transfil_nc(WV)
! READ WAVE TRANSFORMATION DATA
USE GEN_FILE, ONLY : checkNCStatus, nf90_get_varid, fullfile
USE NETCDF, ONLY : nf90_open, nf90_nowrite, nf90_inquire, &
    nf90_inquire_variable, nf90_get_var, nf90_global, nf90_get_att
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(WvModTyp),INTENT(INOUT) :: WV
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'rd_transfil_nc'
INTEGER(KIND=ISGL) :: fid, varid
INTEGER(KIND=ISGL) :: i
INTEGER(KIND=ISGL) :: Ny, Ny_tmp, Nper, Ndir, NHt
INTEGER(KIND=ISGL) :: alloc_stat, tmpstat
INTEGER(KIND=ISGL),DIMENSION(2) :: k
REAL(KIND=SGL) :: kfrac
REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:) :: tmp_ht, tmp_per, tmp_dir, tmp_chain, tmp_dpth, tmp_theta
REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:,:,:,:) :: tmp_K, tmp_D

Ny = WV%Ny
! Open File
CALL message('Trying to open file: '//TRIM(WV%transfil)//' ...','(a\)')
CALL CheckNCStatus(nf90_open(PATH = fullfile(path,WV%transfil,''),&
    MODE = nf90_nowrite,&
    NCID = fid),&
    .TRUE.,'nf90_open')
CALL errchk(sub); IF (errstat) RETURN
CALL message('OK','(a)')
! Get Dimension Size Info
CALL get_dim_size('Ny',Ny_tmp); CALL errchk(sub); IF (errstat) RETURN
CALL get_dim_size('Nper',Nper); CALL errchk(sub); IF (errstat) RETURN
CALL get_dim_size('Ndir',Ndir); CALL errchk(sub); IF (errstat) RETURN
CALL get_dim_size('NHt',NHt); CALL errchk(sub); IF (errstat) RETURN
! Allocate transformation table memory
DO i = 1,Ny
    alloc_stat = 0
    ALLOCATE(WV%trans(i)%Ht(NHt),  STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
    ALLOCATE(WV%trans(i)%Per(Nper),  STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
    ALLOCATE(WV%trans(i)%Dir(Ndir),  STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
    ALLOCATE(WV%trans(i)%K(Ndir,Nper,NHt),  STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
    ALLOCATE(WV%trans(i)%D(Ndir,Nper,NHt),  STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
    IF (alloc_stat/=0) THEN
        CALL err(sub,'Allocate error'); RETURN
    END IF
    WV%trans(i)%NHt = NHt
    WV%trans(i)%Nper = Nper
    WV%trans(i)%Ndir = Ndir
    WV%trans(i)%Ht = 0.
    WV%trans(i)%Per = 0.
    WV%trans(i)%Dir = 0.
    WV%trans(i)%K = 0.
    WV%trans(i)%D = 0.
END DO
! Get Chainage, Depth and Alpha values
CALL CheckNCStatus(nf90_get_att(NCID = fid,&
    VARID = nf90_global,&
    NAME = 'Offshore_Depth',&
    VALUES = WV%Wvoff%dpth),&
    .TRUE.,'nf90_get_att')
CALL errchk(sub); IF (errstat) RETURN
CALL CheckNCStatus(nf90_get_att(NCID = fid,&
    VARID = nf90_global,&
    NAME = 'Offshore_Contour_Rotation',&
    VALUES = WV%Wvoff%theta),&
    .TRUE.,'nf90_get_att')
CALL errchk(sub); IF (errstat) RETURN
! Allocate temporary storage
alloc_stat = 0
ALLOCATE(tmp_ht(Nht), STAT=tmpstat); IF (tmpstat/=0) alloc_stat=tmpstat
ALLOCATE(tmp_per(Nper), STAT=tmpstat); IF (tmpstat/=0) alloc_stat=tmpstat
ALLOCATE(tmp_dir(Ndir), STAT=tmpstat); IF (tmpstat/=0) alloc_stat=tmpstat
ALLOCATE(tmp_chain(Ny_tmp), STAT=tmpstat); IF (tmpstat/=0) alloc_stat=tmpstat
ALLOCATE(tmp_dpth(Ny_tmp), STAT=tmpstat); IF (tmpstat/=0) alloc_stat=tmpstat
ALLOCATE(tmp_theta(Ny_tmp), STAT=tmpstat); IF (tmpstat/=0) alloc_stat=tmpstat
ALLOCATE(tmp_K(Ndir,Nper,NHt,Ny_tmp), STAT=tmpstat); IF (tmpstat/=0) alloc_stat=tmpstat
ALLOCATE(tmp_D(Ndir,Nper,NHt,Ny_tmp), STAT=tmpstat); IF (tmpstat/=0) alloc_stat=tmpstat
IF (alloc_stat/=0) THEN
    CALL err(sub,'Allocate error'); RETURN
END IF
! Get temporary variables to be interpolated/copied at model chainages
CALL get_var_1d('Height',tmp_ht); CALL errchk(sub); IF (errstat) RETURN
CALL get_var_1d('Period',tmp_per); CALL errchk(sub); IF (errstat) RETURN
CALL get_var_1d('Direction',tmp_dir); CALL errchk(sub); IF (errstat) RETURN
CALL get_var_1d('Chainage',tmp_chain); CALL errchk(sub); IF (errstat) RETURN
CALL get_var_1d('Depth',tmp_dpth); CALL errchk(sub); IF (errstat) RETURN
!tmp_dpth = -tmp_dpth ! Convert from elevation into depth
CALL get_var_1d('Contour_Rotation',tmp_theta); CALL errchk(sub); IF (errstat) RETURN
CALL get_var_4d('K',tmp_K); CALL errchk(sub); IF (errstat) RETURN
CALL get_var_4d('D',tmp_D); CALL errchk(sub); IF (errstat) RETURN
! Interpolate/copy to model chainages
DO i = 1,Ny
    WV%trans(i)%Ht = tmp_ht
    WV%trans(i)%Per = tmp_per
    WV%trans(i)%Dir = tmp_dir * deg_to_rad ! Convert to radians
    CALL get_interp1(tmp_chain,WV%chain(i),k,kfrac)
    WV%Wvint(i)%dpth = (1.-kfrac) * tmp_dpth(k(1)) + kfrac * tmp_dpth(k(2))
    WV%Wvint(i)%theta = (1.-kfrac) * tmp_theta(k(1)) + kfrac * tmp_theta(k(2))
    WV%Wvint(i)%theta = WV%Wvint(i)%theta * deg_to_rad ! Convert %theta to radians
    WV%trans(i)%K = (1.-kfrac) * tmp_K(:,:,:,k(1)) + kfrac * tmp_K(:,:,:,k(2))
    WV%trans(i)%D = (1.-kfrac) * tmp_D(:,:,:,k(1)) + kfrac * tmp_D(:,:,:,k(2))
    WV%trans(i)%D = WV%trans(i)%D * deg_to_rad ! Convert %D to radians
END DO
DEALLOCATE(tmp_chain, STAT=tmpstat)
DEALLOCATE(tmp_dpth, STAT=tmpstat)
DEALLOCATE(tmp_theta, STAT=tmpstat)
DEALLOCATE(tmp_K, STAT=tmpstat)
DEALLOCATE(tmp_D, STAT=tmpstat)

CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE get_dim_size(name,size)
USE NETCDF, ONLY : nf90_inq_dimid, nf90_inquire_dimension
CHARACTER(LEN=*),INTENT(IN) :: name
INTEGER(KIND=ISGL),INTENT(INOUT) :: size
!
INTEGER(KIND=ISGL) :: Dimid
CALL CheckNCStatus(nf90_inq_dimid(NCID = fid,&
    NAME = name,&
    DIMID = Dimid),&
    .TRUE.,'nf90_inq_dimid')
CALL errchk(sub); IF (errstat) RETURN
CALL CheckNCStatus(nf90_inquire_dimension(NCID = fid,&
    DIMID = Dimid,&
    LEN = size),&
    .TRUE.,'nf90_inquire_dimension')
CALL errchk(sub); IF (errstat) RETURN
!
END SUBROUTINE get_dim_size
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE get_var_1d(name,var)
USE GEN_FILE, ONLY : nf90_get_varid
USE NETCDF, ONLY : nf90_get_var
CHARACTER(LEN=*),INTENT(IN) :: name
REAL(KIND=SGL),DIMENSION(:),INTENT(INOUT) :: var
!
INTEGER(KIND=ISGL) :: Varid
CALL CheckNCStatus(nf90_get_varid(fid,&
    VARNAME = name,&
    VARID = varid),&
    .TRUE.,'nf90_get_varid')
CALL errchk(sub); IF (errstat) RETURN
CALL CheckNCStatus(nf90_get_var(NCID = fid,&
    VARID = varid,&
    VALUES = var),&
    .TRUE.,'nf90_get_var')
CALL errchk(sub); IF (errstat) RETURN
!
END SUBROUTINE get_var_1d
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE get_var_4d(name,var)
USE GEN_FILE, ONLY : nf90_get_varid
USE NETCDF, ONLY : nf90_get_var
CHARACTER(LEN=*),INTENT(IN) :: name
REAL(KIND=SGL),DIMENSION(:,:,:,:),INTENT(INOUT) :: var
!
INTEGER(KIND=ISGL) :: Varid
CALL CheckNCStatus(nf90_get_varid(fid,&
    VARNAME = name,&
    VARID = varid),&
    .TRUE.,'nf90_get_varid')
CALL errchk(sub); IF (errstat) RETURN
CALL CheckNCStatus(nf90_get_var(NCID = fid,&
    VARID = varid,&
    VALUES = var),&
    .TRUE.,'nf90_get_var')
CALL errchk(sub); IF (errstat) RETURN
!
END SUBROUTINE get_var_4d
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE rd_transfil_nc
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
PURE FUNCTION wvtyp_average(wvtyp_1,wvtyp_2) RESULT(wvtyp_av)
IMPLICIT NONE
! FUNCTION ARGUMENTS
TYPE(WvParTyp),INTENT(IN) :: wvtyp_1, wvtyp_2
! FUNCTION RESULT
TYPE(WvParTyp) :: wvtyp_av

wvtyp_av%dpth = 0.5 * (wvtyp_1%dpth + wvtyp_2%dpth)
wvtyp_av%theta = 0.5 * (wvtyp_1%theta + wvtyp_2%theta)
wvtyp_av%Per = 0.5 * (wvtyp_1%Per + wvtyp_2%Per)
wvtyp_av%Dir = 0.5 * (wvtyp_1%Dir + wvtyp_2%Dir)
wvtyp_av%Ht = 0.5 * (wvtyp_1%Ht + wvtyp_2%Ht)

END FUNCTION wvtyp_average
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE get_interp1(x,xi,k,kfrac)
! Get 1d interpolation indexes (k) and weights (kfrac)
! SUBROUTINE USE STATEMENTS
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
REAL(KIND=SGL),DIMENSION(:),INTENT(IN) :: x
REAL(KIND=SGL),INTENT(IN) :: xi
INTEGER(KIND=ISGL),DIMENSION(2),INTENT(OUT) :: k
REAL(KIND=SGL),INTENT(OUT) :: kfrac
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: N
!
N = SIZE(x)

k(1) = maxloc(x-xi,MASK=x-xi .LE. 0,DIM=1)

IF (k(1)==0) THEN
    k(1) = 1; k(2) = k(1); kfrac = 0.
ELSE IF (k(1)==N) THEN
    k(2) = k(1); kfrac = 0.
ELSE
    k(2) = k(1) + 1
    kfrac = (xi-x(k(1)))/(x(k(2))-x(k(1)))
END IF
!
END SUBROUTINE get_interp1
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
FUNCTION do_interp3(x,k,kfrac) RESULT(xi)
! Do 3D interpolation given indexes (k) and weights (kfrac)
IMPLICIT NONE
! FUNCTION ARGUMENTS
REAL(KIND=SGL),DIMENSION(:,:,:) :: x
INTEGER(KIND=ISGL),DIMENSION(2,3) :: k
REAL(KIND=SGL),DIMENSION(3) :: kfrac
! FUNCTION RESULT
REAL(KIND=SGL) :: xi
! LOCAL VARIABLES
REAL(KIND=SGL),DIMENSION(2,2,2) :: x3
REAL(KIND=SGL),DIMENSION(2,2) :: x2
REAL(KIND=SGL),DIMENSION(2) :: x1
!
x3 = x(k(:,1),k(:,2),k(:,3))
x2 = (1.-kfrac(3)) * x3(:,:,1) + kfrac(3) * x3(:,:,2)
x1 = (1.-kfrac(2)) * x2(:,1) + kfrac(2) * x2(:,2)
xi = (1.-kfrac(1)) * x1(1) + kfrac(1) * x1(2)
!
END FUNCTION do_interp3
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE transform_off(Wvoff,Trans,Wvint)
! Transform waves from offshore to intermediate point using externally generated tables.
! Use of this subroutine assumes that all locations use the same offshore wave conditions and
! that all transformation tables have the same basis.
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(WvParTyp),INTENT(IN) :: Wvoff
TYPE(WvTransTyp),DIMENSION(:),INTENT(IN) :: Trans
TYPE(WvParTyp),DIMENSION(:),INTENT(INOUT) :: Wvint
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: i, Ny
INTEGER(KIND=ISGL),DIMENSION(2,3) :: idx
REAL(KIND=SGL),DIMENSION(3) :: wts
REAL(KIND=SGL) :: K, D

! Get interpolation indexes and weights
CALL get_interp1(Trans(1)%Dir,Wvoff%Dir,idx(:,1),wts(1))
CALL get_interp1(Trans(1)%Per,Wvoff%Per,idx(:,2),wts(2))
CALL get_interp1(Trans(1)%Ht,Wvoff%Ht,idx(:,3),wts(3))

! Loop through chainages
Ny = SIZE(Trans)
DO i = 1,Ny
    K = do_interp3(Trans(i)%K, idx, wts)
    D = do_interp3(Trans(i)%D, idx, wts)
    Wvint(i)%Ht = Wvoff%Ht * K
    Wvint(i)%Per = Wvoff%Per
    Wvint(i)%Dir = Wvoff%Dir + D
END DO

END SUBROUTINE transform_off
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE linear_transform(h1,Ht1,alpha1,T,h2,Ht2,alpha2)
! Perform linear wave transformation assuming straight parallel contours.
USE GEN_UTIL, ONLY : kh_calc
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
REAL(KIND=SGL),INTENT(IN) :: h1
REAL(KIND=SGL),INTENT(IN) :: Ht1
REAL(KIND=SGL),INTENT(IN) :: alpha1
REAL(KIND=SGL),INTENT(IN) :: T
REAL(KIND=SGL),INTENT(IN) :: h2
REAL(KIND=SGL),INTENT(OUT) :: Ht2
REAL(KIND=SGL),INTENT(OUT) :: alpha2
! LOCAL VARIABLES
REAL(KIND=SGL) :: k0h1, kh1, c1, Ks1, k0h2, kh2, c2, Ks2, cos_alpha1, cos_alpha2, Ks, Kr

k0h1 = 4. * pi**2 / g / T**2 * h1
kh1 = kh_calc(k0h1)
c1 = g * T / (2. * pi) * tanh(kh1);
Ks1 = ((1. + 2. * kh1 / sinh(2. * kh1)) * tanh(kh1))**(-0.5)

k0h2 = 4 * pi**2 / g / T**2 * h2
kh2 = kh_calc(k0h2)
c2 = g * T / (2. * pi) * tanh(kh2)
Ks2 = ((1. + 2. * kh2 /sinh(2. * kh2)) * tanh(kh2))**(-0.5)

Ks = Ks2/Ks1

alpha2 = asin( sin(alpha1) * c2 / c1)
cos_alpha1 = cos(alpha1)
cos_alpha2 = cos(alpha2)
Kr = sqrt( cos_alpha1 / cos_alpha2 )

Ht2 = Ks * Kr * Ht1

END SUBROUTINE linear_transform
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE calc_wvbrk_theta(Wvint,theta,WVbrk)
! Calculate smoothed contour angle at wave breaking
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(WvParTyp),DIMENSION(:),INTENT(IN) :: Wvint
REAL(KIND=SGL),DIMENSION(:),INTENT(IN) :: theta
TYPE(WvParTyp),DIMENSION(:),INTENT(INOUT) :: Wvbrk
! LOCAL PARAMETERS
REAL(KIND=SGL),PARAMETER :: pi_on_2 = pi/2.
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: Ny, i, iup, idn
REAL(KIND=SGL) :: alpha, xx, yy, wi, wu

Ny = SIZE(Wvint)
DO i = 2,Ny-1
!    alpha =  Wvint(i)%dir + theta(i) - pi_on_2
!    IF (alpha>0.) THEN
!        iup = i - 1
!        IF (alpha<pi/4.) THEN
!            wi = 1.0
!            wu = 0.0
!        ELSE
!            wi = 0.0
!            wu = 1.0
!        END IF
!    ELSE
!        iup = i + 1
!        IF (alpha>-pi/4.) THEN
!            wi = 1.0
!            wu = 0.0
!        ELSE
!            wi = 0.0
!            wu = 1.0
!        END IF   
!    END IF
!    xx = wi * cos(theta(i)) + wu * cos(theta(iup))
!    yy = wi * sin(theta(i)) + wu * sin(theta(iup))
!    Wvbrk(i)%theta = atan2(yy,xx) ! Smoothed contour rotation
    Wvbrk(i)%theta = theta(i)
END DO
Wvbrk(1)%theta = theta(1)
Wvbrk(Ny)%theta = theta(Ny)

END SUBROUTINE calc_wvbrk_theta
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE transform_brk(Wvint,Wvbrk)
! Transform waves from intermediate depth to breaking
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(WvParTyp),INTENT(IN) :: Wvint
TYPE(WvParTyp),INTENT(INOUT) :: Wvbrk
! LOCAL PARAMETERS
INTEGER(KIND=ISGL),PARAMETER :: Nmax = 10
REAL(KIND=SGL),PARAMETER :: pi_on_2 = pi/2.
REAL(KIND=SGL),PARAMETER :: alphamax = pi/4.
REAL(KIND=SGL),PARAMETER :: wi = 1.0
REAL(KIND=SGL),PARAMETER :: wb = 1.0
REAL(KIND=SGL),PARAMETER :: small = EPSILON(0.)
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: n
REAL(KIND=SGL) :: xx, yy, theta, h1, Ht1, alpha1, T, h2, Ht2, alpha2, hb, err, sgn_alpha

xx = wi * cos(Wvint%theta) + wb * cos(Wvbrk%theta)
yy = wi * sin(Wvint%theta) + wb * sin(Wvbrk%theta)
theta = atan2(yy,xx) ! Effective contour rotation
h1 = Wvint%dpth
Ht1 = Wvint%Ht
alpha1 =  Wvint%dir + theta - pi_on_2
sgn_alpha = SIGN(1.,alpha1)
alpha1 = ABS(alpha1)
T = Wvint%Per
IF (alpha1<pi_on_2) THEN
!    IF (alpha1>alphamax) alpha1 = alphamax
    h2 = Ht1 / breaker_index
    DO n = 1,Nmax
        CALL linear_transform(h1,Ht1,alpha1,T,h2,Ht2,alpha2)
        hb = Ht2 / breaker_index
        err = (hb - h2)/MAX(0.01,hb)
        IF (err<0.01) THEN
            hb = 0.5*(hb+h2)
            EXIT
        ELSE
            h2 = 0.5*(hb+h2)
        END IF
    END DO
    Wvbrk%dpth = hb
    Wvbrk%Ht = Ht2
    Wvbrk%dir = pi_on_2 + SIGN(alpha2, sgn_alpha) - theta
    Wvbrk%Per = T
ELSE
    Wvbrk%dpth = small
    Wvbrk%Ht = small
    Wvbrk%dir = pi_on_2 + SIGN(alpha1, sgn_alpha) - theta
    Wvbrk%Per = T
END IF

END SUBROUTINE transform_brk
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE wv_update(WV,Wvoff,theta_shore)
! UPDATE WAVE MODEL
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(WVmodtyp),INTENT(INOUT) :: WV
TYPE(WvParTyp),INTENT(IN) :: Wvoff
REAL(KIND=SGL),DIMENSION(:),INTENT(IN) :: theta_shore
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'wv_update'
INTEGER(KIND=ISGL) :: i

! Check inputs
IF (SIZE(theta_shore)/=WV%Ny) THEN
    CALL err(sub,'theta_shore input argument is not size compatible with WV object'); RETURN
END IF
! Set offshore wave conditions
WV%Wvoff = WVoff
! Transform to intermediate wave conditions (using transformation tables)
CALL transform_off(WV%Wvoff,WV%trans,WV%Wvint)
! Calculate smoothed contour angle at wave breaking
CALL calc_wvbrk_theta(WV%Wvint,theta_shore,WV%Wvbrk)
! Transform to wave breaking
DO i = 1,WV%Ny
    CALL transform_brk(WV%Wvint(i),WV%Wvbrk(i))
END DO

END SUBROUTINE wv_update
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE wv_output(wv)
! OUTPUT WAVE MODEL RESULTS
USE GEN_UTIL, ONLY : time2str
USE GEN_FILE, ONLY : openfile
USE GEN_STRING, ONLY : pad, num2str, strtrim
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(WvModTyp),INTENT(INOUT) :: wv
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'wv_output'
INTEGER(KIND=ISGL) :: i

DO i = 1,wv%Nout
    CALL do_output(wv%out(i))
END DO

CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE do_output(out)
! PROCESS A SINGLE OUTPUT REQUEST
TYPE(GenOutTyp),INTENT(INOUT) :: out
! LOCAL VARIABLES

IF (out%t > t+0.0001_DBL .OR. out%t > out%tfinal) RETURN

SELECT CASE(out%typ)
CASE(1) ! OFFSHORE WAVES
    CALL message('    Writing offshore wave csv output.','(a\)')
    CALL do_offshore_wave_output(out)
CASE(2,3) ! INTERMEDIATE/BREAKING WAVES
    IF (out%typ==2) THEN
        CALL message('    Writing intermediate wave csv output.','(a\)')
        CALL do_wave_output(out,WV%Wvint)
    ELSE
        CALL message('    Writing breaking wave csv output.','(a\)')
        CALL do_wave_output(out,WV%Wvbrk)
    END IF
END SELECT

IF (tform==0) THEN
    CALL message(' t = '//TRIM(time2str(t,tform,tzero))//' days.','(a)')
ELSE
    CALL message(' t = '//TRIM(time2str(t,tform,tzero))//'.','(a)')
END IF

out%t = MAX(out%t,t) + REAL(out%dt,KIND=DBL)

END SUBROUTINE do_output
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE do_offshore_wave_output(out)
TYPE(GenOutTyp),INTENT(INOUT) :: out
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: i, ierr

IF (.NOT.out%init) THEN
    CALL openfile(UNIT=out%fid,FILE=out%fil,STATUS=pad('REPLACE',15),&
                ACCESS=pad('SEQUENTIAL',15),ACTION=pad('READWRITE',15),&
                FORM=pad('FORMATTED',15))
    CALL errchk(sub); IF (errstat) RETURN
    WRITE(out%fid, '(a)',IOSTAT=ierr) 'TIME,WVHT,WVPER,WVDIR'
    out%init = .TRUE.
END IF

WRITE(out%fid,'(a\)',IOSTAT=ierr) strtrim(time2str(t,tform,tzero))
WRITE(out%fid,'(","a","a","a)',IOSTAT=ierr) &
    strtrim(num2str(WV%Wvoff%Ht)), &
    strtrim(num2str(WV%Wvoff%Per)), &
    strtrim(num2str(WV%Wvoff%Dir * rad_to_deg))

END SUBROUTINE do_offshore_wave_output
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE do_wave_output(out,wvtyp)
TYPE(GenOutTyp),INTENT(INOUT) :: out
TYPE(WvParTyp),DIMENSION(:),INTENT(IN) :: wvtyp
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: i, j, ierr

IF (.NOT.out%init) THEN
    CALL openfile(UNIT=out%fid,FILE=out%fil,STATUS=pad('REPLACE',15),&
                ACCESS=pad('SEQUENTIAL',15),ACTION=pad('READWRITE',15),&
                FORM=pad('FORMATTED',15))
    CALL errchk(sub); IF (errstat) RETURN
    WRITE(out%fid, '(a\)',IOSTAT=ierr) 'VARIABLE,TIME/CHAINAGE'
    DO j = 1,WV%Ny
        WRITE(out%fid, '(a\)',IOSTAT=ierr) ','//strtrim(num2str(WV%chain(j)))
    END DO
    WRITE(out%fid, '(a)',IOSTAT=ierr)
    out%init = .TRUE.
END IF
WRITE(out%fid,'(a\)',IOSTAT=ierr) 'WvHt'
WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(time2str(t,tform,tzero))
DO j = 1,WV%Ny
    WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(num2str(wvtyp(j)%ht))
END DO
WRITE(out%fid,'(a)',IOSTAT=ierr)
WRITE(out%fid,'(a\)',IOSTAT=ierr) 'WvPer'
WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(time2str(t,tform,tzero))
DO j = 1,WV%Ny
    WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(num2str(wvtyp(j)%Per))
END DO
WRITE(out%fid,'(a)',IOSTAT=ierr)
WRITE(out%fid,'(a\)',IOSTAT=ierr) 'WvDir'
WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(time2str(t,tform,tzero))
DO j = 1,WV%Ny
    WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(num2str(wvtyp(j)%Dir * rad_to_deg))
END DO
WRITE(out%fid,'(a)',IOSTAT=ierr)

END SUBROUTINE do_wave_output
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE wv_output
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

END MODULE WVmod
