! QUASI-2D SHORELINE EVOLUTION MODULE

MODULE EVOmod

! MODULE USE STATEMENTS
USE PRECISION
USE GEN_ERROR
USE LOG
USE GLOBALS
USE WVmod, ONLY : WvParTyp, WVmodtyp
USE LSmod, ONLY : LSmodtyp, Groynetyp
USE XSmod, ONLY : XSmodtyp
USE BCmod, ONLY : BCtyp
USE OUTmod, ONLY : GenOutTyp

! MODULE DECLARATIONS
IMPLICIT NONE
!$ INCLUDE "omp_lib.h"

! MODULE ACCESS
PRIVATE
PUBLIC :: EVOtyp, construct, destruct, initialise, run_timestep, run

! MODULE INTERFACES
INTERFACE construct
    MODULE PROCEDURE evo_construct
END INTERFACE
INTERFACE destruct
    MODULE PROCEDURE evo_destruct
END INTERFACE
INTERFACE initialise
    MODULE PROCEDURE evo_initialise
END INTERFACE
INTERFACE run_timestep
    MODULE PROCEDURE evo_run_timestep
END INTERFACE
INTERFACE run
    MODULE PROCEDURE evo_run
END INTERFACE

! MODULE TYPES
! GEO SUB-TYPE
TYPE GEOtyp
    INTEGER(KIND=ISGL) :: Ny                                        ! Number of faces in evo model
    REAL(KIND=DBL),ALLOCATABLE,DIMENSION(:,:) :: base               ! Baseline coordinates (mEast,mNorth) [2,Ny]
    REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:) :: length               ! Profile lengths [Ny]
    REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:) :: chain                ! Baseline chainages (m) [Ny]
    REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:) :: eta                  ! Local coordinate rotation (rad) [Ny]
    REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:,:) :: dy                 ! Baseline/end distance between profiles (m) [2,Ny-1]
    REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:,:) :: seawall            ! Seawall definition [2,Ny-1]
    TYPE(Groynetyp),ALLOCATABLE,DIMENSION(:) :: groynes                        ! Groyne structure definition [Ng]
END TYPE
! XSgrptyp
TYPE XSgrptyp
    INTEGER(KIND=ISGL) :: Nxs                                       ! Nxs = Ny - 1
    REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:) :: dVdt                 ! Rate of volume change (m^3 s^-1) [Nxs]
    TYPE(XSmodtyp),ALLOCATABLE,DIMENSION(:) :: xs                   ! Xshore model objects [Nxs]
    TYPE(GenOutTyp),ALLOCATABLE,DIMENSION(:) :: out                 ! Xshore model output requests [Nxs]
END TYPE
! EVO TYPE
TYPE EVOtyp
    TYPE(GEOtyp),ALLOCATABLE :: geo                                 ! Model geometry sub-object
    TYPE(BCtyp),ALLOCATABLE,DIMENSION(:) :: bc                      ! Model boundary conditions sub-object [Nbc]
    TYPE(WvParTyp) :: Wvoff                                         ! Offshore wave conditions
    REAL(KIND=SGL) :: wl                                            ! Water Level
    TYPE(WVmodtyp),ALLOCATABLE :: wv                                ! Wave model sub-object
    TYPE(LSmodtyp),ALLOCATABLE :: ls                                ! Longshore model sub-object
    TYPE(XSgrptyp),ALLOCATABLE :: xsgrp                             ! Xshore sub-object
    REAL(KIND=SGL) :: dt                                            ! Model update timestep
    REAL(KIND=SGL) :: dt_dsp                                        ! Model display timestep
    REAL(KIND=SGL) :: dt_rst                                        ! Write restart timestep
    REAL(KIND=DBL) :: t_dsp                                         ! Next display time
    REAL(KIND=DBL) :: t_rst                                         ! Next write restart time
END TYPE

! MODULE PARAMETERS
INTEGER(KIND=ISGL),PARAMETER :: Nparmax = 100
INTEGER(KIND=SGL),PARAMETER :: Nlmax = 10

! MODULE VARIABLES

! MODULE PROCEDURES
CONTAINS
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE evo_construct(EV,ctrl)
! EVO OBJECT CONSTRUCTOR ROUTINE (public)
USE EVOctrl, ONLY : ctrl_typ
USE BCmod, ONLY : construct
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(EVOtyp),INTENT(OUT) :: EV
TYPE(ctrl_typ),INTENT(IN) :: ctrl
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'evo_construct'
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: Nl
INTEGER(KIND=ISGL) :: tmpstat, alloc_stat
REAL(KIND=SGL) :: dx, dy

CALL message('Constructing EVO object:','(a/)')

SELECT CASE(ctrl%modtyp)
CASE (0) ! Simple
    Nl = 1
CASE (1) ! Huxley
    Nl = 2
CASE DEFAULT
    CALL err(sub, 'Requested model type is not supported'); RETURN
END SELECT

! CONSTRUCT MODEL GEOMETRY
CALL message('Constructing model geometry...','(a)')
ALLOCATE(EV%geo, STAT=alloc_stat)
IF (alloc_stat/=0) THEN
    CALL err(sub,'Allocate error'); RETURN
END IF
CALL geo_construct(EV%geo,ctrl%modtyp,ctrl%grdfil,ctrl%swallfil,ctrl%groynfil)
CALL errchk(sub); IF (errstat) RETURN
CALL message('Finished constructing model geometry.','(a/)')

! CONSTRUCT XS SUB-OBJECT
CALL message('Constructing XS sub-models...','(a)')
CALL xs_construct()
CALL errchk(sub); IF (errstat) RETURN
CALL message('Finished constructing XS sub-models.','(a/)')

! CONSTRUCT LS SUB-OBJECT
CALL message('Constructing LS sub-model...','(a)')
CALL ls_construct()
CALL errchk(sub); IF (errstat) RETURN
CALL message('Finished constructing LS sub-model.','(a/)')

! CONSTRUCT WV SUB-OBJECT
CALL message('Constructing WV sub-model...','(a)')
CALL wv_construct()
CALL errchk(sub); IF (errstat) RETURN
CALL message('Finished constructing WV sub-model.','(a/)')

! CONSTRUCT BC SUB-OBJECT
CALL message('Constructing BC sub-object...','(a)')
CALL construct(EV%bc,ctrl%bc,EV%geo%chain)
CALL errchk(sub); IF (errstat) RETURN
CALL message('Constructing BC sub-object.','(a/)')

EV%dt = ctrl%dt
EV%dt_dsp = ctrl%dt_dsp
EV%dt_rst = ctrl%dt_rst
EV%t_dsp = t
EV%t_rst = t + REAL(EV%dt_rst,DBL)

CALL message('Succesfully constructed EVO object.','(a/)')

CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE xs_construct()
USE XSmod, ONLY : construct
USE GEN_FILE, ONLY : openfile, csvread
USE GEN_UTIL, ONLY : interp1
USE GEN_STRING, ONLY : pad, num2str, strtrim
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: i, j, Nxs, Npar, Ntmp, Nout
INTEGER(KIND=ISGL) :: out_id
INTEGER(KIND=ISGL) :: alloc_stat, ierr
INTEGER(KIND=ISGL),ALLOCATABLE,DIMENSION(:,:) :: outptr
REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:,:) :: xpartmp
REAL(KIND=SGL),DIMENSION(Nparmax) :: xpar
REAL(KIND=SGL) :: chain, length
REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:,:) :: outtmp
CHARACTER(LEN=MSTR),DIMENSION(Nparmax) :: header

! Allocate xs sub-object (Ny-1)
ALLOCATE(EV%xsgrp, STAT=alloc_stat)
EV%xsgrp%Nxs = EV%geo%Ny-1
ALLOCATE(EV%xsgrp%xs(EV%xsgrp%Nxs), STAT=alloc_stat);
ALLOCATE(EV%xsgrp%dVdt(EV%xsgrp%Nxs), STAT=alloc_stat);
IF (alloc_stat/=0) THEN
    CALL err(sub,'Allocate error'); RETURN
END IF

! Read parameter file (model specific)
CALL message('Reading xsect parameter file...','(a)')
SELECT CASE(ctrl%modtyp)
CASE (0) ! Simple
    Npar = 9
    header(1:Npar+1) = (/'CHAINAGE','DUNE_ELEVATION','DUNE_SLOPE','SHORELINE_HEIGHT','ACTIVE_HEIGHT','ACTIVE_SLOPE','TRANSITION_SLOPE','OFFSHORE_SLOPE','OFFSHORE_ELEVATION','K'/)
CASE (1) ! Huxley
    Npar = 9
    header(1:Npar+1) = (/'CHAINAGE','DUNE_ELEVATION','DUNE_SLOPE','A','TRANSITION_SLOPE','OFFSHORE_SLOPE','OFFSHORE_ELEVATION','KE','KA','HBMIN'/)
CASE DEFAULT
    CALL err(sub,'Requested model type is not currently implemented.'); RETURN   
END SELECT
CALL csvread(ctrl%xparfil,header(1:Npar+1),xpartmp)
CALL errchk(sub); IF (errstat) RETURN

! Process xs group output requests
Ntmp = SIZE(ctrl%out,1)
! Count XS model related outputs
Nout = 0
DO i = 1,Ntmp
    SELECT CASE (ctrl%out(i)%typnam)
    CASE('XSECT','EQUILIBRIUMXSECT','XSHORE','VOLUME')
        Nout = Nout + 1
    END SELECT
END DO
! Allocate ev%xsgrp%out
ALLOCATE(outptr(Nout,EV%xsgrp%Nxs),STAT=alloc_stat)
ALLOCATE(ev%xsgrp%out(Nout),STAT=alloc_stat)
IF (alloc_stat/=0) THEN
    CALL err(sub, 'Allocate error.')
    RETURN
END IF
! Initialise values
Nout = 0
outptr = 0
DO i = 1,Ntmp
    ! Set integer type
    SELECT CASE (ctrl%out(i)%typnam)
    CASE('XSECT')
        Nout = Nout + 1
        ev%xsgrp%out(Nout)%typ = 1
    CASE('EQUILIBRIUMXSECT')
        Nout = Nout + 1
        ev%xsgrp%out(Nout)%typ = 2
    CASE('XSHORE')
        Nout = Nout + 1
        ev%xsgrp%out(Nout)%typ = 3
    CASE('VOLUME')
        Nout = Nout + 1
        ev%xsgrp%out(Nout)%typ = 4
    CASE DEFAULT
        CYCLE
    END SELECT
    ! Set standard values
    ev%xsgrp%out(Nout)%OutCtrlTyp = ctrl%out(i)
    ev%xsgrp%out(Nout)%fid = 0
    ev%xsgrp%out(Nout)%init = .FALSE.
    ev%xsgrp%out(Nout)%t = ctrl%out(i)%tstart
    ! Read additional info
    SELECT CASE (ctrl%out(i)%typnam)
    CASE('XSECT','EQUILIBRIUMXSECT')
        ! Read x-sect id list
        CALL csvread(ev%xsgrp%out(Nout)%info,(/'ID'/),outtmp)
        CALL errchk(sub); IF (errstat) RETURN
        IF(SIZE(outtmp,2)<1) THEN
            CALL err(sub,'No xsect_id specified for output.'); RETURN
        END IF
        ALLOCATE(ev%xsgrp%out(Nout)%obj(1),STAT=alloc_stat)
        ALLOCATE(ev%xsgrp%out(Nout)%obj(1)%id(SIZE(outtmp,2)),STAT=alloc_stat)
        IF (alloc_stat/=0) THEN
            CALL err(sub, 'Allocate error.')
            RETURN
        END IF
        ev%xsgrp%out(Nout)%obj(1)%id = outtmp(1,:)

!       CALL sort(ev%xsgrp%out(Nout)%obj(1)%id)

        outptr(Nout,ev%xsgrp%out(Nout)%obj(1)%id) = Nout
    END SELECT
END DO

! Interpolate parameters and construct each cross-section object
DO i = 1,EV%xsgrp%Nxs
    chain = 0.5 * (EV%geo%chain(i)+EV%geo%chain(i+1))
    length = 0.5 * (EV%geo%length(i)+EV%geo%length(i+1))
    DO j = 1,Npar
        xpar(j) = interp1(xpartmp(1,:),xpartmp(j+1,:),chain)
    END DO
    IF (ANY(outptr(:,i)>0)) THEN
        CALL construct( EV%xsgrp%xs(i),ctrl%modtyp,xpar(1:Npar),length,ID=i,DY=EV%geo%dy(:,i),WALL=EV%geo%seawall(:,i), &
                        OUTPUT=ev%xsgrp%out(PACK(outptr(:,i),outptr(:,i)>0))                                            )
    ELSE
        CALL construct( EV%xsgrp%xs(i),ctrl%modtyp,xpar(1:Npar),length,ID=i,DY=EV%geo%dy(:,i),WALL=EV%geo%seawall(:,i)  )
    END IF
    Ev%xsgrp%dVdt(i) = 0.
    CALL errchk(sub); IF (errstat) RETURN
END DO

END SUBROUTINE xs_construct
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE ls_construct()
USE LSmod, ONLY : construct, set_parameters
USE GEN_FILE, ONLY : csvread
USE GEN_STRING, ONLY : num2str
! LOCAL VARIABLES

! Allocate scalar ls sub-object
ALLOCATE(EV%ls, STAT=alloc_stat)
IF (alloc_stat/=0) THEN
    CALL err(sub,'Allocate error'); RETURN
END IF

! Construct longshore model
CALL construct(EV%ls,Nl,EV%geo%base,EV%geo%eta,GROYNES=EV%geo%groynes,OUTPUT=ctrl%out)
CALL errchk(sub); IF (errstat) RETURN

! Set model parameters
CALL set_parameters(EV%ls,ctrl%ls_param)
CALL errchk(sub); IF (errstat) RETURN

END SUBROUTINE ls_construct
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE wv_construct()
USE WVmod, ONLY : construct
! LOCAL VARIABLES

ALLOCATE(EV%wv, STAT=alloc_stat);
IF (alloc_stat/=0) THEN
    CALL err(sub,'Allocate error'); RETURN
END IF
CALL construct(EV%wv,EV%ls%chain,ctrl%WvTransFil,OUTPUT=ctrl%out)
CALL errchk(sub); IF (errstat) RETURN

END SUBROUTINE wv_construct
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE evo_construct
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE evo_destruct(EV)
! EVO OBJECT DESTRUCTOR ROUTINE (public)
USE WVmod, ONLY : destruct
USE LSmod, ONLY : destruct
USE XSmod, ONLY : destruct
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(EVOtyp),INTENT(INOUT) :: EV
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'evo_destruct'
INTEGER(KIND=ISGL) :: i
INTEGER(KIND=ISGL) :: tmpstat

! WV SUB-OBJECT
CALL destruct(EV%wv)
CALL errchk(sub); IF (errstat) RETURN
DEALLOCATE(EV%wv, STAT=tmpstat)
! XS SUB-OBJECT
DO i = 1,SIZE(EV%xsgrp%xs)
    CALL destruct(EV%xsgrp%xs(i))
    CALL errchk(sub); IF (errstat) RETURN
END DO
DEALLOCATE(EV%xsgrp%out, STAT=tmpstat)
DEALLOCATE(EV%xsgrp%xs, STAT=tmpstat)
DEALLOCATE(EV%xsgrp, STAT=tmpstat)
! LS SUB-OBJECT
CALL destruct(EV%ls)
CALL errchk(sub); IF (errstat) RETURN
DEALLOCATE(EV%ls, STAT=tmpstat)
! GEO SUB-OBJECT
CALL geo_destruct(EV%geo)
CALL errchk(sub); IF (errstat) RETURN
DEALLOCATE(EV%geo, STAT=tmpstat)

END SUBROUTINE evo_destruct 
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE geo_construct(geo,typ,geofil,swallfil,groynfil)
! CONSTRUCT EVO MODEL GEOMETRY SUB-OBJECT
USE GEN_FILE, ONLY : csvread
USE GEN_GEO, ONLY : distance
! SUBROUTINE ARGUMENTS
TYPE(GEOtyp),INTENT(OUT) :: geo
INTEGER(KIND=ISGL),INTENT(IN) :: typ
CHARACTER(LEN=*),INTENT(IN) :: geofil
CHARACTER(LEN=*),INTENT(IN) :: swallfil
CHARACTER(LEN=*),INTENT(IN) :: groynfil
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub  = 'geo_construct'
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: i, Ny
INTEGER(KIND=ISGL) :: alloc_stat, tmpstat
INTEGER(KIND=ISGL) :: fid
REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:,:) :: grid
REAL(KIND=SGL) :: dx, dy

! Read data from geofil
CALL csvread(geofil,(/'BASE_X','BASE_Y','END_X','END_Y'/),grid)
CALL errchk(sub); IF (errstat) RETURN
Ny = SIZE(grid,2)
alloc_stat = 0
ALLOCATE(geo%base(2,Ny), STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
ALLOCATE(geo%length(Ny), STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
ALLOCATE(geo%chain(Ny), STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
ALLOCATE(geo%eta(Ny), STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
ALLOCATE(geo%dy(2,Ny), STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
ALLOCATE(geo%seawall(2,Ny-1), STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
IF (alloc_stat/=0) THEN
    CALL err(sub,'Allocate error'); RETURN
END IF
geo%Ny = Ny
DO i = 1,Ny
    geo%base(:,i) = grid(1:2,i)
    IF (i==1) THEN
        geo%chain(i) = 0.
    ELSE
        geo%dy(1,i-1) = distance(grid(1,i),grid(2,i),grid(1,i-1),grid(2,i-1))
        geo%dy(2,i-1) = distance(grid(3,i),grid(4,i),grid(3,i-1),grid(4,i-1))
        geo%chain(i) = geo%chain(i-1) + geo%dy(1,i-1)
    END IF
    dx = grid(3,i) - grid(1,i)
    dy = grid(4,i) - grid(2,i)
    geo%length(i) = SQRT(dx**2+dy**2)
    geo%eta(i) = ATAN2(dy,dx)
END DO

! Read seawall definition file
IF (swallfil/='') CALL construct_seawall

! Read groyne definition file
IF (groynfil/='') CALL construct_groyne

CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE construct_seawall
USE GEN_UTIL, ONLY : interp1
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: i, j
REAL(KIND=SGL) :: chain
REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:,:) :: walltmp
REAL(KIND=SGL),DIMENSION(2) :: seawall

CALL message('Reading seawall definition file...','(a)')
CALL  csvread(swallfil,(/'CHAINAGE','WALL_Z','WALL_X'/),walltmp)
CALL errchk(sub); IF (errstat) RETURN

DO i = 1,Ny-1
    chain = 0.5 * (geo%chain(i)+geo%chain(i+1))
    DO j = 1,2
        geo%seawall(j,i) = interp1(walltmp(1,:),walltmp(j+1,:),chain)
    END DO
END DO

END SUBROUTINE construct_seawall
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE construct_groyne
USE GEN_STRING, ONLY : num2str
USE BCmod, ONLY : read_evo_file
! LOCAL VARIABLES
CHARACTER(LEN=30),DIMENSION(4) :: head = (/'CHAINAGE','GROYNE_X','START_TIME','END_TIME'/)
LOGICAL,DIMENSION(4) :: is_time = (/.FALSE.,.FALSE.,.TRUE.,.TRUE./)
INTEGER(KIND=ISGL) :: i, n, Ng
CHARACTER(LEN=MSTR),DIMENSION(2) :: header
REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:,:) :: groyntmp

CALL message('Reading groyne definition file...','(a)')
CALL read_evo_file(groynfil,head,is_time,groyntmp)
CALL errchk(sub); IF (errstat) RETURN
Ng = COUNT(groyntmp(2,:)>0.)
CALL message(num2str(Ng)//' groynes identified in file.','(a)')
ALLOCATE(geo%groynes(Ng), STAT=alloc_stat)
IF (alloc_stat/=0) THEN
    CALL err(sub,'Allocate error'); RETURN
END IF
i = 0
DO n = 1,SIZE(groyntmp,2)
    IF (groyntmp(2,n)>0.) THEN
        i = i + 1
        geo%groynes(i)%id = 0
        geo%groynes(i)%chain = groyntmp(1,n)
        geo%groynes(i)%xg = groyntmp(2,n)
        IF (.NOT.isnan(groyntmp(3,n))) THEN
            geo%groynes(i)%start_time = groyntmp(3,n)
        ELSE
            geo%groynes(i)%start_time = -HUGE(0._DBL)
        END IF
        IF (.NOT.isnan(groyntmp(4,n))) THEN
            geo%groynes(i)%end_time = groyntmp(4,n)
        ELSE
            geo%groynes(i)%end_time = HUGE(0._DBL)
        END IF
    END IF
END DO

END SUBROUTINE construct_groyne
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
END SUBROUTINE geo_construct
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE geo_destruct(geo)
! DESTRUCT EVO MODEL GEOMETRY SUB-OBJECT
IMPLICIT NONE
TYPE(GEOtyp),INTENT(INOUT) :: geo
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'geo_destruct'
INTEGER(KIND=ISGL) :: tmpstat

DEALLOCATE(geo%base, STAT=tmpstat)
DEALLOCATE(geo%length, STAT=tmpstat)
DEALLOCATE(geo%chain, STAT=tmpstat)
DEALLOCATE(geo%eta, STAT=tmpstat)
DEALLOCATE(geo%dy, STAT=tmpstat)
DEALLOCATE(geo%groynes, STAT=tmpstat)
DEALLOCATE(geo%seawall, STAT=tmpstat)
geo%Ny = 0

END SUBROUTINE geo_destruct
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE evo_write_restart(EV)
! WRITE RESTART FILE
USE GEN_FILE, ONLY : openfile, closefile, fullfile
USE GEN_STRING, ONLY : pad, num2str
! SUBROUTINE ARGUMENTS
TYPE(EVOtyp),INTENT(IN) :: EV
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'evo_write_restart'
INTEGER(KIND=ISGL) :: fid, i, ierr
CHARACTER(LEN=LSTR) :: fname

fname = fullfile(logdir,evc,'.rst')
CALL openfile(UNIT=fid,&
    FILE=fname,&
    ACCESS=pad('SEQUENTIAL',15),&
    ACTION=pad('WRITE',15),&
    FORM=pad('BINARY',15))
WRITE(fid,IOSTAT=ierr) t
WRITE(fid,IOSTAT=ierr) EV%xsgrp%Nxs
WRITE(fid,IOSTAT=ierr) EV%xsgrp%xs(1)%Nvar
DO i = 1,EV%xsgrp%Nxs
    WRITE(fid,IOSTAT=ierr) REAL(EV%xsgrp%xs(i)%xvar,KIND=SGL)
    WRITE(fid,IOSTAT=ierr) REAL(EV%xsgrp%xs(i)%zvar,KIND=SGL)
END DO
CALL closefile(UNIT=fid)
IF (ierr/=0) THEN
    CALL err(sub,'error writing restart file '//num2str(fid)); RETURN
END IF

END SUBROUTINE evo_write_restart
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE evo_read_restart(EV,fname,t_rst,x_rst,z_rst)
! WRITE RESTART FILE
USE GEN_FILE, ONLY : openfile, closefile, fullfile
USE GEN_STRING, ONLY : pad, num2str
! SUBROUTINE ARGUMENTS
TYPE(EVOtyp),INTENT(IN) :: EV
CHARACTER(LEN=*),INTENT(IN) :: fname
REAL(KIND=DBL),INTENT(OUT) :: t_rst
REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:,:),INTENT(OUT) :: x_rst
REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:,:),INTENT(OUT) :: z_rst
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'evo_read_restart'
INTEGER(KIND=ISGL) :: fid, i, ierr
INTEGER(KIND=ISGL) :: Nxs, Nvar
INTEGER(KIND=ISGL) :: tmpstat, alloc_stat

IF (fname=='') RETURN

CALL openfile(UNIT=fid,&
    FILE=fname,&
    ACCESS=pad('SEQUENTIAL',15),&
    ACTION=pad('READ',15),&
    FORM=pad('BINARY',15))
CALL errchk(sub); IF (errstat) RETURN
READ(fid,IOSTAT=ierr) t_rst
READ(fid,IOSTAT=ierr) Nxs
IF (Nxs /= EV%xsgrp%Nxs) THEN
    CALL err(sub,'Restart file not compatible with number of model cross-sections')
    RETURN
END IF
READ(fid,IOSTAT=ierr) Nvar
IF (Nvar /= EV%xsgrp%xs(1)%Nvar) THEN
    CALL err(sub,'Restart file not compatible with cross-shore model type')
    RETURN
END IF
alloc_stat = 0
ALLOCATE(x_rst(Nvar,Nxs), STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
ALLOCATE(z_rst(Nvar,Nxs), STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
IF (alloc_stat/=0) THEN
    CALL err(sub,'allocate error'); RETURN
END IF
DO i = 1,EV%xsgrp%Nxs
    READ(fid,IOSTAT=ierr) x_rst(:,i)
    READ(fid,IOSTAT=ierr) z_rst(:,i)
END DO
CALL closefile(UNIT=fid)
IF (ierr/=0) THEN
    CALL err(sub,'error reading from restart file.'); RETURN
END IF

END SUBROUTINE evo_read_restart
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE evo_initialise(EV,ctrl)
! INITIALISE EVO OBJECT
USE EVOctrl, ONLY : ctrl_typ
USE LSmod, ONLY : initialise
USE XSmod, ONLY : initialise
USE GEN_STRING, ONLY : num2str
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(EVOtyp),INTENT(INOUT) :: EV
TYPE(ctrl_typ),INTENT(IN) :: ctrl
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'evo_initialise'
INTEGER(KIND=ISGL) :: i, j, Nl, Ny, Ntmp
INTEGER(KIND=ISGL) :: alloc_stat, tmpstat
REAL(KIND=DBL) :: t_rst
REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:,:) :: x_init
REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:,:) :: z_init

CALL message('Setting model initial conditions...','(a)')

Nl = EV%ls%Nl
Ny = EV%ls%Ny

! Get initial conditions from restart file
IF (ctrl%rstfil/='') THEN
    ! Get initial conditions from restart file
    CALL evo_read_restart(EV,ctrl%rstfil,t_rst,x_init,z_init)
    CALL errchk(sub); IF (errstat) RETURN
    IF (.NOT. ctrl%reset_t) CALL update_t(t_rst)
ELSE
    ! Get initial conditions from initial condition file
    CALL read_ic_file(EV,ctrl%initcondfil,x_init,z_init)
    CALL errchk(sub); IF (errstat) RETURN
END IF

! Initialise ls sub-model
CALL initialise(EV%ls,x_init)
CALL errchk(sub); IF (errstat) RETURN

! Update bc & wv components
EV%wvoff = EV%wv%wvoff
CALL update_bc(EV)
CALL errchk(sub); IF (errstat) RETURN
CALL update_wv(EV)
CALL errchk(sub); IF (errstat) RETURN

! Initialise xs sub-model
DO i = 1,EV%xsgrp%Nxs
    CALL initialise(EV%xsgrp%xs(i), x_init(:,i), z_init(:,i), EV%wl, EV%wv%wvbrk(i))
    CALL errchk(sub)
    IF (errstat) THEN
        CALL message('Error has occurred initialising ''XSmod'' for cross-section '//num2str(i),'(/a)')
        RETURN
    END IF
END DO

! Update ls component
CALL update_ls(EV)
CALL errchk(sub); IF (errstat) RETURN

! Write initial output
CALL evo_output(EV)
CALL errchk(sub); IF (errstat) RETURN

! Deallocate temporary variables
DEALLOCATE(x_init, STAT=tmpstat)
DEALLOCATE(z_init, STAT=tmpstat)

CALL message('Succesfully set model initial conditions.','(a/)')

CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE read_ic_file(EV,fname,x_init,z_init)
USE GEN_STRING, ONLY : num2str
USE GEN_FILE, ONLY : csvread
USE GEN_UTIL, ONLY : interp1
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(EVOtyp),INTENT(IN) :: EV
CHARACTER(LEN=*),INTENT(IN) :: fname
REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:,:),INTENT(OUT) :: x_init
REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:,:),INTENT(OUT) :: z_init
! LOCAL VARIABLES
INTEGER(KIND=ISGL) ::Nl, Ny, Nxs
REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:,:) :: tmpdat
REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:) :: chain_tmp
REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:,:) :: x_init_tmp
REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:,:) :: z_init_tmp
CHARACTER(LEN=MSTR),DIMENSION(Nlmax) :: header

Nl = EV%ls%Nl
Ny = EV%ls%Ny
Nxs = EV%xsgrp%Nxs

! Read initial condition file
header(1) = 'Chainage'
DO j = 1,Nl
    header(2*j) = 'X'//num2str(j-1)
    header(2*j+1) = 'Z'//num2str(j-1)
END DO
CALL csvread(fname,header(1:2*Nl+1),tmpdat)

! Allocate temporary variables
Ntmp = SIZE(tmpdat,2)
alloc_stat = 0
ALLOCATE(chain_tmp(Ntmp), STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
ALLOCATE(x_init_tmp(Nl,Ntmp), STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
ALLOCATE(z_init_tmp(Nl,Ntmp), STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
IF (alloc_stat/=0) THEN
    CALL err(sub,'allocate error'); RETURN
END IF
chain_tmp = tmpdat(1,:)
x_init_tmp = tmpdat(2:2*Nl:2,:)
z_init_tmp = tmpdat(3:2*Nl+1:2,:)

alloc_stat = 0
ALLOCATE(x_init(Nl,Nxs), STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
ALLOCATE(z_init(Nl,Nxs), STAT=tmpstat); IF (tmpstat/=0) alloc_stat = tmpstat
IF (alloc_stat/=0) THEN
    CALL err(sub,'allocate error'); RETURN
END IF

DO i = 1,Nxs
    DO j = 1,Nl
        x_init(j,i) = interp1(chain_tmp, x_init_tmp(j,:), 0.5 * (EV%ls%chain(i) + EV%ls%chain(i+1)))
        z_init(j,i) = interp1(chain_tmp, z_init_tmp(j,:), 0.5 * (EV%ls%chain(i) + EV%ls%chain(i+1)))
    END DO
END DO

! Deallocate temporary variables
DEALLOCATE(chain_tmp, STAT=tmpstat)
DEALLOCATE(x_init_tmp, STAT=tmpstat)
DEALLOCATE(z_init_tmp, STAT=tmpstat)

END SUBROUTINE read_ic_file
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE evo_initialise
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE update_bc(EV)
USE BCmod, ONLY : update
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(EVOtyp),INTENT(INOUT) :: EV
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: i

CALL update(EV%bc)

! Transfer data from bcs
DO i = 1,SIZE(EV%bc)
    SELECT CASE(EV%bc(i)%typ)
    CASE (1) ! WAVE
        EV%wvoff%Ht = EV%bc(i)%tdat(1)
        EV%wvoff%Per = EV%bc(i)%tdat(2)
        EV%wvoff%Dir = EV%bc(i)%tdat(3) * deg_to_rad
    CASE (2) ! WL
        EV%wl = EV%bc(i)%tdat(1)
    END SELECT
END DO

END SUBROUTINE update_bc
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE update_wv(EV)
USE WVmod, ONLY : update, output
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(EVOtyp),INTENT(INOUT) :: EV
! LOCAL VARIABLES
REAL(KIND=SGL),DIMENSION(EV%wv%Ny) :: thetab

thetab = EV%ls%thetab + EV%ls%eta ! Convert local coordinate thetab into global coordinates
CALL update(EV%wv,EV%wvoff,thetab)

END SUBROUTINE update_wv
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE update_ls(EV)
USE XSmod, ONLY : update
USE LSmod, ONLY : update
USE Wvmod, ONLY : WvParTyp, average
USE GEN_STRING, ONLY : num2str
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(EVOtyp),INTENT(INOUT) :: EV
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: i, Ny
REAL(KIND=SGL),DIMENSION(EV%ls%Nl,EV%ls%Ny-1) :: xl
REAL(KIND=SGL),DIMENSION(EV%ls%Ny-1) :: x0, x0a, xbr
REAL(KIND=SGL) :: dVmax
TYPE(WvParTyp) :: wvbrk

Ny = EV%geo%Ny
! Transfer data from xshore models
DO i = 1,Ny-1
    wvbrk = average(EV%wv%wvbrk(i), EV%wv%wvbrk(i+1))
    CALL update(EV%xsgrp%xs(i),EV%wl,wvbrk)
    xl(:,i) = EV%xsgrp%xs(i)%xvar
    x0(i) = EV%xsgrp%xs(i)%x0
    x0a(i) = EV%xsgrp%xs(i)%x0a
    xbr(i) = EV%xsgrp%xs(i)%xbr
END DO
! Update LS object - calculate longshore transport rates
IF (do_ls_update) THEN
    CALL update(EV%ls,EV%wv%wvbrk,xl,x0,x0a,xbr,EV%bc)
END IF
!! Apply conservative volume redistribution
!DO i = 1,Ny-1
!    dVmax =  EV%xsgrp%xs(i)%prof(1)%vol - EV%xsgrp%xs(i)%prof(1)%vol_min
!    IF (-EV%LS%Qs(i)*EV%dt>dVmax) THEN
!        EV%LS%Qs(i) = -dVmax / EV%dt
!        CALL warn('Conservative volume redistribution applied at cross-section '//num2str(i))
!    END IF
!    IF (EV%LS%Qs(i+1)*EV%dt>dVmax) THEN
!        EV%LS%Qs(i+1) = dVmax / EV%dt
!        CALL warn('Conservative volume redistribution applied at cross-section '//num2str(i))
!    END IF
!END DO
    

END SUBROUTINE update_ls
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE update_xs(EV)
USE XSmod, ONLY : update
USE Wvmod, ONLY : WvParTyp, average
USE GEN_STRING, ONLY : num2str, strtrim
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(EVOtyp),INTENT(INOUT) :: EV
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'update_xs'
INTEGER(KIND=ISGL) :: i, nbc

IF (.NOT.do_xs_update) RETURN

DO i = 1,EV%xsgrp%Nxs
    EV%xsgrp%dVdt(i) = EV%LS%Qs(i)-EV%LS%Qs(i+1)
    EV%xsgrp%dVdt(i) = EV%xsgrp%dVdt(i) * morfac
END DO

DO nbc = 1,SIZE(EV%bc)
    SELECT CASE (EV%bc(nbc)%typ)
    CASE (4) ! SOURCE/SINK
        CALL apply_sourcesink_bc(EV%bc(nbc))
    END SELECT
END DO

DO i = 1,EV%xsgrp%Nxs
    CALL update(EV%xsgrp%xs(i),EV%xsgrp%dVdt(i),EV%dt)
    CALL errchk(sub)
    IF (errstat) THEN
        CALL message('Error has occurred in ''XSmod'' for cross-section '//num2str(i),'(/a)')
        CALL message('Requested volume change is '//strtrim(num2str(EV%xsgrp%dVdt(i)*EV%dt)),'(a)')
        CALL message('Transport 1 is '//strtrim(num2str(EV%LS%Qs(i)))//' m^3/s','(a)')
        CALL message('Transport 2 is '//strtrim(num2str(EV%LS%Qs(i+1)))//' m^3/s','(a/)')
        RETURN
    END IF
END DO

CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE apply_sourcesink_bc(bc)
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(BCtyp),INTENT(IN) :: bc
! LOCAL PARMETERS
REAL(KIND=SGL) :: convert_transport = 365.25 * 24. * 3600.

! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: n, i, i1, i2
REAL(KIND=SGL) :: frac

DO n = 1,SIZE(bc%obj)
    i1 = bc%obj(n)%id(1)
    i2 = bc%obj(n)%id(2)
    frac = 1. / REAL(i2 - i1 + 1, SGL)
    DO i = i1,i2
        EV%xsgrp%dVdt(i) = EV%xsgrp%dVdt(i) + bc%tdat(n) * frac / convert_transport
    END DO
END DO

END SUBROUTINE apply_sourcesink_bc
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE update_xs
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE xsgrp_output(xsgrp,geo)
! Process xsgroup output commands

USE XSmod, ONLY : output
USE GEN_UTIL, ONLY : time2str
USE GEN_FILE, ONLY : openfile
USE GEN_STRING, ONLY : pad, num2str, strtrim
! SUBROUTINE ARGUMENTS
TYPE(XSgrptyp),INTENT(INOUT) :: xsgrp
TYPE(GeoTyp),INTENT(IN) :: geo
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'xsgrp_output'
INTEGER(KIND=ISGL) :: i
INTEGER(KIND=ISGL) :: ierr

DO i = 1,SIZE(xsgrp%out)
    CALL do_output(xsgrp%out(i))
END DO

DO i = 1,SIZE(xsgrp%xs)
    CALL output(xsgrp%xs(i),ECHO=.FALSE.)
    CALL errchk(sub); IF (errstat) RETURN
END DO

CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE do_output(out)
! Process a single output request
! SUBROUTINE ARGUMENTS
TYPE(GenOutTyp),INTENT(INOUT) :: out
! LOCAL VARIABLES

IF (out%t > t+0.0001_DBL .OR. out%t > out%tfinal) RETURN

SELECT CASE(out%typ)
CASE(1) ! XSECT OUTPUT
    CALL message('    Writing xsect csv output.','(a\)')
    CALL do_xsect_output(out)
CASE(2) ! EQUILIBRIUM XSECT OUTPUT
    CALL message('    Writing equilibrium xsect csv output.','(a\)')
    CALL do_xsect_output(out)
CASE(3) ! XSHORE OUTPUT
    CALL message('    Writing xshore csv output.','(a\)')
    CALL do_xshore_output(out)
CASE(4) ! VOLUME OUTPUT
    CALL message('    Writing volume csv output.','(a\)')
    CALL do_vol_output(out)
END SELECT

IF (tform==0) THEN
    CALL message(' t = '//TRIM(time2str(t,tform,tzero))//' days.','(a)')
ELSE
    CALL message(' t = '//TRIM(time2str(t,tform,tzero))//'.','(a)')
END IF

out%t = MAX(out%t,t) + REAL(out%dt,KIND=DBL)

END SUBROUTINE do_output
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE do_xsect_output(out)
! SUBROUTINE ARGUMENTS
TYPE(GenOutTyp),INTENT(INOUT) :: out
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: j, Nxs, id

Nxs = SIZE(out%obj(1)%id)
! Open output files
IF (.NOT.out%init) THEN
    CALL openfile(UNIT=out%fid,FILE=out%fil,&
        STATUS=pad('REPLACE',15),ACCESS=pad('SEQUENTIAL',15),&
        ACTION=pad('READWRITE',15),FORM=pad('FORMATTED',15))
    CALL errchk(sub); IF (errstat) RETURN
    WRITE(out%fid, '(a\)',IOSTAT=ierr) 'XSECT_ID,VARIABLE,TIME/POINT'
    DO j = 1,xsgrp%xs(1)%prof(1)%Nx
        WRITE(out%fid, '(a\)') ','//strtrim(num2str(j))
    END DO
    WRITE(out%fid, '(a)')
    out%init = .TRUE.
    DO j = 1,Nxs
        id = out%obj(1)%id(j)
        xsgrp%xs(id)%out%fid = out%fid
        xsgrp%xs(id)%out%init = .TRUE.
    END DO
END IF

END SUBROUTINE do_xsect_output
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE do_xshore_output(out)
! SUBROUTINE ARGUMENTS
TYPE(GenOutTyp),INTENT(INOUT) :: out
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: j, n
INTEGER(KIND=ISGL) :: ierr
REAL(KIND=SGL) :: chain

IF (.NOT.out%init) THEN
    CALL openfile(UNIT=out%fid,FILE=out%fil,STATUS=pad('REPLACE',15),&
                ACCESS=pad('SEQUENTIAL',15),ACTION=pad('READWRITE',15),&
                FORM=pad('FORMATTED',15))
    CALL errchk(sub); IF (errstat) RETURN
    WRITE(out%fid, '(a\)',IOSTAT=ierr) 'VARIABLE,TIME/CHAINAGE'
    DO j = 1,xsgrp%Nxs
        chain = 0.5 * (geo%chain(j)+geo%chain(j+1))
        WRITE(out%fid, '(a\)',IOSTAT=ierr) ','//strtrim(num2str(chain))
    END DO
    WRITE(out%fid, '(a)',IOSTAT=ierr)
    out%init = .TRUE.
END IF

WRITE(out%fid,'(a\)',IOSTAT=ierr) 'XWL'
WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(time2str(t,tform,tzero))
DO j = 1,xsgrp%Nxs
    WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(num2str(xsgrp%xs(j)%x0))
END DO
WRITE(out%fid,'(a)',IOSTAT=ierr)

WRITE(out%fid,'(a\)',IOSTAT=ierr) 'ZWL'
WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(time2str(t,tform,tzero))
DO j = 1,xsgrp%Nxs
    WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(num2str(xsgrp%xs(j)%wl))
END DO
WRITE(out%fid,'(a)',IOSTAT=ierr)

DO n = 1,xsgrp%xs(1)%Nvar
    WRITE(out%fid,'(a\)',IOSTAT=ierr) 'X'//num2str(n-1)
    WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(time2str(t,tform,tzero))
    DO j = 1,xsgrp%Nxs
        WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(num2str(xsgrp%xs(j)%xvar(n)))
    END DO
    WRITE(out%fid,'(a)',IOSTAT=ierr)
    WRITE(out%fid,'(a\)',IOSTAT=ierr) 'Z'//num2str(n-1)
    WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(time2str(t,tform,tzero))
    DO j = 1,xsgrp%Nxs
        WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(num2str(xsgrp%xs(j)%zvar(n)))
    END DO
    WRITE(out%fid,'(a)',IOSTAT=ierr)
END DO

END SUBROUTINE do_xshore_output
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE do_vol_output(out)
! SUBROUTINE ARGUMENTS
TYPE(GenOutTyp),INTENT(INOUT) :: out
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: j, n
INTEGER(KIND=ISGL) :: ierr
REAL(KIND=SGL) :: chain

IF (.NOT.out%init) THEN
    CALL openfile(UNIT=out%fid,FILE=out%fil,STATUS=pad('REPLACE',15),&
                ACCESS=pad('SEQUENTIAL',15),ACTION=pad('READWRITE',15),&
                FORM=pad('FORMATTED',15))
    CALL errchk(sub); IF (errstat) RETURN
    WRITE(out%fid, '(a\)',IOSTAT=ierr) 'VARIABLE,TIME/CHAINAGE'
    DO j = 1,xsgrp%Nxs
        chain = 0.5 * (geo%chain(j)+geo%chain(j+1))
        WRITE(out%fid, '(a\)',IOSTAT=ierr) ','//strtrim(num2str(chain))
    END DO
    WRITE(out%fid, '(a)',IOSTAT=ierr)
    out%init = .TRUE.
END IF

WRITE(out%fid,'(a\)',IOSTAT=ierr) 'VOL_UPPER_PER_M'
WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(time2str(t,tform,tzero))
DO j = 1,xsgrp%Nxs
    WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(num2str(xsgrp%xs(j)%prof(1)%vol_upper_per_m))
END DO
WRITE(out%fid,'(a)',IOSTAT=ierr)

WRITE(out%fid,'(a\)',IOSTAT=ierr) 'VOL_PER_M'
WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(time2str(t,tform,tzero))
DO j = 1,xsgrp%Nxs
    WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(num2str(xsgrp%xs(j)%prof(1)%vol_per_m))
END DO
WRITE(out%fid,'(a)',IOSTAT=ierr)

WRITE(out%fid,'(a\)',IOSTAT=ierr) 'VOL'
WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(time2str(t,tform,tzero))
DO j = 1,xsgrp%Nxs
    WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(num2str(xsgrp%xs(j)%prof(1)%vol))
END DO
WRITE(out%fid,'(a)',IOSTAT=ierr)

WRITE(out%fid,'(a\)',IOSTAT=ierr) 'VOL_ERROR'
WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(time2str(t,tform,tzero))
DO j = 1,xsgrp%Nxs
    WRITE(out%fid,'(","a\)',IOSTAT=ierr) strtrim(num2str(xsgrp%xs(j)%prof(1)%vol_error))
END DO
WRITE(out%fid,'(a)',IOSTAT=ierr)

END SUBROUTINE do_vol_output
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE xsgrp_output
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE evo_output(EV)
! CALL SUB-MODEL OUTPUT ROUTINES
USE WVmod, ONLY : output
USE LSmod, ONLY : output
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(EVOtyp),INTENT(INOUT) :: EV
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'evo_output'
INTEGER(KIND=ISGL) :: i

CALL output(EV%wv)
CALL errchk(sub); IF (errstat) RETURN
CALL output(EV%ls)
CALL errchk(sub); IF (errstat) RETURN
CALL xsgrp_output(EV%xsgrp,EV%geo)
CALL errchk(sub); IF (errstat) RETURN

END SUBROUTINE evo_output
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE evo_run_timestep(EV)
! EXECUTE SINGLE TIMESTEP UPDATE OF EVO OBJECT
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(EVOtyp),INTENT(INOUT) :: EV
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'evo_run_timestep'
INTEGER(KIND=ISGL) :: i

CALL update_bc(EV)
CALL errchk(sub); IF (errstat) RETURN
CALL update_wv(EV)
CALL errchk(sub); IF (errstat) RETURN
CALL update_ls(EV)
CALL errchk(sub); IF (errstat) RETURN
CALL update_xs(EV)
CALL errchk(sub); IF (errstat) RETURN

CALL evo_output(EV)
CALL errchk(sub); IF (errstat) RETURN

CALL update_t(t + EV%dt)

END SUBROUTINE evo_run_timestep
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE evo_run(EV,ctrl)
! EXECUTE EVO MODEL TIMESTEPS
USE EVOCTRL, ONLY : ctrl_typ
USE GEN_STRING, ONLY : num2str, strtrim
USE GEN_UTIL, ONLY : time2str
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(EVOtyp),INTENT(INOUT) :: EV
TYPE(ctrl_typ),INTENT(IN) :: ctrl
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'evo_run'
REAL(KIND=DBL) :: t1, t2

CALL message('Entering timestep loop...','(a/)')

! Initialise time variables
EV%t_dsp = t
EV%t_rst = t + REAL(EV%dt_rst,DBL)
!DEC$ IF .NOT.DEFINED(_OPENMP)
CALL CPU_TIME(t1)
!DEC$ ELSE
t1 = OMP_GET_WTIME()
!DEC$ END IF

! Timestep loop
DO
    
    ! Loop exit
    IF (t>ctrl%tend-0.0001_DBL) EXIT
    
    CALL update_nstep()

    ! Display timestep information
    CALL display_timestep()
   
    ! Execute timestep methods
    CALL evo_run_timestep(EV)
    CALL errchk(sub); IF (errstat) RETURN
    
    ! Write restart file
    IF (t > EV%t_rst-0.0001_DBL) THEN
        CALL evo_write_restart(EV)
        CALL errchk(sub); IF (errstat) RETURN
        EV%t_rst = EV%t_rst + REAL(EV%dt_rst,DBL)
    END IF

END DO

! Write final outputs
CALL evo_output(EV)
CALL errchk(sub); IF (errstat) RETURN
CALL evo_write_restart(EV)
CALL errchk(sub); IF (errstat) RETURN

! Display final timestep info
CALL display_final_timestep()

CALL message('Run successful.','(a/)')

CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE display_timestep()
IF (t > EV%t_dsp-0.0001_DBL) THEN
!DEC$ IF .NOT.DEFINED(_OPENMP)
    CALL CPU_TIME(t2)
!DEC$ ELSE
    t2 = OMP_GET_WTIME()
!DEC$ END IF
    IF (tform==0) THEN
        CALL message('t = '//strtrim(time2str(t,tform,tzero))//' days.','(a\)')
    ELSEIF (tform==1) THEN
        CALL message('t = '//strtrim(time2str(t,tform,tzero))//'.','(a\)')
    END IF
    CALL message('  elapsed time = '//strtrim(num2str(t2-t1,'(F9.1)'))// ' s.','(a\)')
    CALL message('  nstep = '//strtrim(num2str(nstep))//'.','(a)')
    EV%t_dsp = EV%t_dsp + REAL(ctrl%dt_dsp,DBL)
END IF
END SUBROUTINE display_timestep
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE display_final_timestep()
!DEC$ IF .NOT.DEFINED(_OPENMP)
CALL CPU_TIME(t2)
!DEC$ ELSE
t2 = OMP_GET_WTIME()
!DEC$ END IF
CALL message('Finished timestep loop.','(a/)')
CALL message('Number of timesteps executed = '//num2str(nstep),'(a)')
!DEC$ IF .NOT.DEFINED(_OPENMP)
CALL message('Elapsed cpu_time = '//num2str(t2-t1)//' s','(a)')
!DEC$ ELSE
CALL message('Elapsed system_time = '//num2str(t2-t1)//' s','(a)')
!DEC$ END IF
END SUBROUTINE display_final_timestep
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE evo_run
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

END MODULE EVOmod
