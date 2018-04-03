MODULE BCmod

! MODULE USE STATEMENTS
USE PRECISION
USE GEN_ERROR
USE LOG
USE GLOBALS


! MODULE DECLARATIONS
IMPLICIT NONE

! MODULE ACCESS
PRIVATE
PUBLIC :: BCtyp, BcCtrlTyp, construct, destruct, update, read_evo_file

! MODULE INTERFACES
INTERFACE construct
    MODULE PROCEDURE bc_construct
END INTERFACE
INTERFACE destruct
    MODULE PROCEDURE bc_destruct
END INTERFACE
INTERFACE update
    MODULE PROCEDURE bc_update
END INTERFACE

! OBJ_LIST TYPE
TYPE :: object_list
    CHARACTER(LEN=MSTR) :: name                                     ! object name
    INTEGER(KIND=ISGL),ALLOCATABLE,DIMENSION(:) :: id               ! object id/s
END TYPE
! BC CONTROL SUB-TYPE
TYPE BcCtrlTyp
    CHARACTER(LEN=MSTR) :: typnam                                   ! Boundary condition type
    CHARACTER(LEN=LSTR) :: fil                                      ! Data file
    REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:) :: chain                ! Chainage list (if required)
    LOGICAL :: loop                                                 ! Loop status
END TYPE
! BC TYPE
TYPE,EXTENDS(BcCtrlTyp) :: BCtyp
    INTEGER(KIND=ISGL) :: typ                                       ! Type id
    TYPE(object_list),ALLOCATABLE,DIMENSION(:) :: obj               ! Object list/s
    REAL(KIND=DBL),ALLOCATABLE,DIMENSION(:) :: time                 ! Time vector [Nt]
    REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:,:) :: dat                ! Data matrix [Ndat,Nt]
    REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:) :: tdat                 ! Instantaneous data vector [Ndat]
END TYPE

! MODULE PARAMETERS
INTEGER(KIND=ISGL),PARAMETER :: Nheadmax = 100

! MODULE VARIABLES

! MODULE PROCEDURES
CONTAINS
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE bc_construct(bc,bc_ctrl,chainage)
! CONSTRUCT EVO MODEL BC SUB-TYPE
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(BCtyp),ALLOCATABLE,DIMENSION(:),INTENT(OUT) :: bc
TYPE(BcCtrlTyp),DIMENSION(:),INTENT(IN) :: bc_ctrl
REAL(KIND=SGL),DIMENSION(:),INTENT(IN) :: chainage
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'bc_construct'
INTEGER(KIND=ISGL) :: nbc, i
INTEGER(KIND=ISGL) :: alloc_stat

nbc = SIZE(bc_ctrl)

! Allocate
ALLOCATE(bc(nbc), STAT=alloc_stat)
IF (alloc_stat/=0) THEN
    CALL err(sub,'Allocate error'); RETURN
END IF
! Initialise
bc%typ = 0
bc%fil = ''
bc%loop = .FALSE.

DO i = 1,nbc
    SELECT CASE (bc_ctrl(i)%typnam)
    CASE ('WAVE')
        bc(i)%typ = 1
        bc(i)%typnam = bc_ctrl(i)%typnam
        bc(i)%fil = bc_ctrl(i)%fil
        bc(i)%loop = bc_ctrl(i)%loop
        CALL init_bc_csv(bc(i),bc_ctrl(i))
        CALL errchk(sub); IF (errstat) RETURN
    CASE ('WATERLEVEL')
        bc(i)%typ = 2
        bc(i)%typnam = bc_ctrl(i)%typnam
        bc(i)%fil = bc_ctrl(i)%fil
        bc(i)%loop = bc_ctrl(i)%loop
        CALL init_bc_csv(bc(i),bc_ctrl(i))
        CALL errchk(sub); IF (errstat) RETURN
    CASE ('TRANSPORT')
        bc(i)%typ = 3
        bc(i)%typnam = bc_ctrl(i)%typnam
        bc(i)%fil = bc_ctrl(i)%fil
        bc(i)%loop = bc_ctrl(i)%loop
        ALLOCATE(bc(i)%chain(1))
        bc(i)%chain(1) = bc_ctrl(i)%chain(1)
        ALLOCATE(bc(i)%obj(1))
        bc(i)%obj(1)%name = 'face id'
        ALLOCATE(bc(i)%obj(1)%id(1), STAT=alloc_stat)
        CALL get_face_id(bc(i)%chain(1),chainage,bc(i)%obj(1)%id(1))
        CALL init_bc_csv(bc(i),bc_ctrl(i))
        CALL errchk(sub); IF (errstat) RETURN
    CASE ('SOURCE/SINK')
        bc(i)%typ = 4
        bc(i)%typnam = bc_ctrl(i)%typnam
        bc(i)%fil = bc_ctrl(i)%fil
        CALL init_sourcesink_bc(bc(i),chainage)
        CALL errchk(sub); IF (errstat) RETURN
    CASE ('BYPASS')
        bc(i)%typ = 5
        bc(i)%typnam = bc_ctrl(i)%typnam
        bc(i)%fil = bc_ctrl(i)%fil
        CALL init_bypass_bc(bc(i),chainage)
        CALL errchk(sub); IF (errstat) RETURN
    CASE DEFAULT
        CALL err(sub,'unknown bc type'); RETURN
    END SELECT
END DO

CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE init_bc_csv(bc,ctrl_bc)
USE GEN_FILE, ONLY : csvread
! SUBROUTINE ARGUMENTS
TYPE(BCtyp),INTENT(INOUT) :: bc
TYPE(BcCtrlTyp),INTENT(IN) :: ctrl_bc
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'init_bc_csv'
INTEGER(KIND=ISGL) :: Ncol
INTEGER(KIND=ISGL) :: alloc_stat
CHARACTER(LEN=MSTR),DIMENSION(Nheadmax) :: header

! Assign column headers
SELECT CASE(bc%typ)
CASE(1) ! WAVE BC
    Ncol = 4
    header(1) = 'TIME'
    header(2) = 'WVHT'
    header(3) = 'WVPER'
    header(4) = 'WVDIR'
CASE(2) ! WL BC
    Ncol = 2
    header(1) = 'TIME'
    header(2) = 'WL'
CASE(3) ! TRANSPORT BC
    Ncol = 2
    header(1) = 'TIME'
    header(2) = 'TRANSPORT'
END SELECT

! Read CSV file
CALL csvread(bc%fil, header(1:Ncol), bc%dat, bc%time, tform, tzero)
CALL errchk(sub); IF (errstat) RETURN

! Allocate memory for bc%tdat
ALLOCATE(bc%tdat(SIZE(bc%dat,1)), STAT=alloc_stat)
IF (alloc_stat/=0) THEN
    CALL err(sub,'Allocate error'); RETURN
END IF
bc%tdat = 0.

IF (bc%loop) THEN
    IF (bc%time(1)>t) THEN
        bc%time = bc%time - bc%time(1) + t ! Rewind looped bc if required
    END IF
END IF

END SUBROUTINE init_bc_csv
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE get_face_id(location,chainages,face_id)
! SUBROUTINE ARGUMENTS
REAL(KIND=SGL),INTENT(IN) :: location
REAL(KIND=SGL),DIMENSION(:),INTENT(IN) :: chainages
INTEGER(KIND=ISGL),INTENT(OUT) :: face_id
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: i
REAL(KIND=SGL) :: min_dist, dist

min_dist = HUGE(0._SGL)
DO i = 1,SIZE(chainages)
    dist = SQRT((location-chainages(i))**2)
    IF (dist>min_dist) THEN
        face_id = i - 1
        RETURN
    ELSE
        min_dist = dist
    END  IF
END DO
face_id = SIZE(chainages)

END SUBROUTINE get_face_id
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE get_xsect_id_range(chainage_limits,chainages,xsect_id_range)
! SUBROUTINE ARGUMENTS
REAL(KIND=SGL),DIMENSION(2),INTENT(IN) :: chainage_limits
REAL(KIND=SGL),DIMENSION(:),INTENT(IN) :: chainages
INTEGER(KIND=ISGL),DIMENSION(2),INTENT(OUT) :: xsect_id_range
! LOCAL VARIABLES

xsect_id_range(1) = maxloc(chainages-chainage_limits(1),MASK=chainages-chainage_limits(1) .LE. 0,DIM=1) ! closest grid before source/sink start location
xsect_id_range(1) = MAX(xsect_id_range(1), 1)								! minimum is point 1
xsect_id_range(2) = maxloc(chainages-chainage_limits(2),MASK=chainages-chainage_limits(2) .LE. 0,DIM=1) ! closest grid before source/sink end location
xsect_id_range(2) = MAX(xsect_id_range(2)-1, xsect_id_range(1))						! not clear why we remove 1 here?

END SUBROUTINE get_xsect_id_range
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE init_sourcesink_bc(bc,chainage)
USE GEN_STRING, ONLY : num2str
! SUBROUTINE ARGUMENTS
TYPE(BCtyp),INTENT(INOUT) :: bc
REAL(KIND=SGL),DIMENSION(:) :: chainage
! LOCAL VARIABLES
CHARACTER(LEN=30),DIMENSION(6) :: head = (/'START_CHAINAGE','END_CHAINAGE','START_TIME','END_TIME','RATE','VOLUME'/)
LOGICAL,DIMENSION(6) :: is_time = (/.FALSE.,.FALSE.,.TRUE.,.TRUE.,.FALSE.,.FALSE./)
INTEGER(KIND=ISGL) :: n, Nss
INTEGER(KIND=ISGL) :: alloc_stat

CALL read_evo_file(bc%fil, head, is_time, bc%dat)
CALL errchk(sub); IF (errstat) RETURN
Nss = SIZE(bc%dat,2)
ALLOCATE(bc%obj(Nss),STAT=alloc_stat)
DO n = 1,Nss
    bc%obj(n)%name = 'Source/Sink '//num2str(n)//' xsect id range'
    ALLOCATE(bc%obj(n)%id(2), STAT=alloc_stat)
    CALL get_xsect_id_range(bc%dat(1:2,n),chainage,bc%obj(n)%id)
END DO
ALLOCATE(bc%tdat(Nss),STAT=alloc_stat)
bc%tdat = 0.

END SUBROUTINE init_sourcesink_bc
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE init_bypass_bc(bc,chainage)
USE GEN_STRING, ONLY : num2str
! SUBROUTINE ARGUMENTS
TYPE(BCtyp),INTENT(INOUT) :: bc
REAL(KIND=SGL),DIMENSION(:) :: chainage
! LOCAL VARIABLES
CHARACTER(LEN=30),DIMENSION(7) :: head = (/'EXTRACTION_CHAINAGE','PLACEMENT_START_CHAINAGE','PLACEMENT_END_CHAINAGE','START_TIME','END_TIME','DIRECTION','PROPORTION'/)
LOGICAL,DIMENSION(7) :: is_time = (/.FALSE.,.FALSE.,.FALSE.,.TRUE.,.TRUE.,.FALSE.,.FALSE./)
INTEGER(KIND=ISGL) :: n, Nss
INTEGER(KIND=ISGL) :: alloc_stat

CALL read_evo_file(bc%fil, head, is_time, bc%dat)
CALL errchk(sub); IF (errstat) RETURN
Nss = SIZE(bc%dat,2)
ALLOCATE(bc%obj(Nss),STAT=alloc_stat)
DO n = 1,Nss
    bc%obj(n)%name = 'Source/Sink '//num2str(n)//' xsect id range'
    ALLOCATE(bc%obj(n)%id(2), STAT=alloc_stat)
    CALL get_xsect_id_range(bc%dat(1:2,n),chainage,bc%obj(n)%id)
END DO
ALLOCATE(bc%tdat(Nss),STAT=alloc_stat)
bc%tdat = 0.

END SUBROUTINE init_bypass_bc
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE bc_construct
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE bc_destruct(bc)
! DESTRUCT EVO MODEL BC SUB-TYPE
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(BCtyp),ALLOCATABLE,DIMENSION(:),INTENT(INOUT) :: bc
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: i, tmpstat

DO i = 1,SIZE(bc)
    DEALLOCATE(bc(i)%tdat, STAT=tmpstat)
    DEALLOCATE(bc(i)%dat, STAT=tmpstat)
END DO
DEALLOCATE(bc, STAT=tmpstat)

END SUBROUTINE bc_destruct
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE read_evo_file(file, head, is_time, dat)
! SUBROUTINE USE STATEMENTS
USE GEN_FILE, ONLY : openfile, closefile
USE GEN_UTIL, ONLY : timein
USE GEN_STRING, ONLY : pad, num2str, upper, strtrim, strrep, strmatch, strfind
! SUBROUTINE DECLARATIONS
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
CHARACTER(LEN=*),INTENT(IN) :: file
CHARACTER(LEN=30),DIMENSION(:),INTENT(IN) :: head
LOGICAL,DIMENSION(:),INTENT(IN) :: is_time
REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:,:),INTENT(OUT) :: dat
! LOCAL PARAMETERS
INTEGER(KIND=ISGL),PARAMETER :: VLSTR = 4000
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'read_evo_file'
INTEGER(KIND=ISGL) :: unit
INTEGER(KIND=ISGL) :: Nhead
INTEGER(KIND=ISGL) :: Ncol
INTEGER(KIND=ISGL) :: Nline
CHARACTER(LEN=MSTR),ALLOCATABLE,DIMENSION(:) :: csvhead
INTEGER(KIND=ISGL),DIMENSION(SIZE(head)) :: col
INTEGER(KIND=ISGL) :: ind1, ind2
INTEGER(KIND=ISGL) :: n, i
CHARACTER(LEN=VLSTR) :: line
CHARACTER(LEN=19) :: timestr
CHARACTER(LEN=LSTR) :: tmpstr
INTEGER(KIND=ISGL) :: tmpstat, ierr
INTEGER(KIND=ISGL) :: alloc_stat, dealloc_stat

! OPEN FILE
CALL openfile(UNIT=unit,FILE=file,&
                STATUS=pad('OLD',15),ACTION=pad('READ',15),FORM=pad('FORMATTED',15))
CALL errchk(sub); IF (errstat) RETURN

! READ HEADER LINE
line = ''
READ(unit,'(a)',IOSTAT=ierr) line
IF (ierr/=0) THEN
    CALL err(sub,'error reading file'); RETURN
END IF
IF (line(VLSTR:VLSTR) /= '') THEN
    CALL warn('maximum number of characters allowed per line in csv file is '//num2str(VLSTR))
END IF
! COUNT NUMBER OF COLUMNS
Ncol = 0
DO
    i = INDEX(line,',',.FALSE.)
    Ncol = Ncol + 1
    IF (i==0) EXIT
    line = line(i+1:)
END DO
! ALLOCATE CSVHEAD
alloc_stat = 0
ALLOCATE(csvhead(Ncol),STAT=alloc_stat)
IF (alloc_stat/=0) THEN
    CALL err(sub,'allocate error'); RETURN
END IF
csvhead = ''
! READ CSV FILE COLUMN HEADERS
REWIND(unit)
READ(unit,'(a)',IOSTAT=ierr) line
IF (ierr/=0) THEN
    CALL err(sub,'error reading file'); RETURN
END IF
DO n = 1,Ncol
    i = INDEX(line,',',.FALSE.)
    IF (i==0) THEN
        READ(line(1:), '(a)') csvhead(n)
    ELSEIF (i==1) THEN
        csvhead(n) = ''
    ELSE
        READ(line(1:i-1), '(a)') csvhead(n)
    END IF
    csvhead(n) = strtrim(upper(csvhead(n)))
    line = line(i+1:)
END DO
! COUNT NUMBER OF LINES
Nline = 0
DO
    READ(unit,'(a)', IOSTAT=ierr) line
    IF (ierr/=0) EXIT
    tmpstr = strrep(line,',','')
    IF (tmpstr=='') EXIT
    Nline = Nline + 1
END DO
! REWIND FILE
REWIND(unit)

! ALLOCATE DAT MATRIX (NLINE,NHEAD)
Nhead = SIZE(head)
ALLOCATE(dat(Nhead,Nline), STAT=alloc_stat)
IF (alloc_stat/=0) THEN
    CALL err(sub,'allocate error'); RETURN
END IF
dat = NAN_SGL

! GET COLUMN NUMBERS CORRESPONDING TO BC HEADERS
tmpstr = ''
DO n = 1,Nhead
    col(n) = strmatch(csvhead,upper(head(n)),.FALSE.)
    IF (col(n)==0) THEN
        CALL err(sub,TRIM(upper(head(n)))//' column not found in input file.')
    ELSE
        tmpstr = TRIM(tmpstr)//TRIM(head(n))//','
    END IF
END DO

! DEALLOCATE CSVHEAD
DEALLOCATE(csvhead, STAT=dealloc_stat)
IF (dealloc_stat/=0) THEN
    CALL err(sub,'deallocate error'); RETURN
END IF
! GET PAST HEADER LINE
READ(unit, '(a)', IOSTAT=ierr) line
! NOW READ DATA FROM FILE
i = 0
DO
    i = i + 1
    IF (i>Nline) EXIT
    READ(unit, '(a)', IOSTAT=ierr) line
    IF (ierr/=0) THEN
        CALL err(sub,'error reading input file'); EXIT
    END IF
    ierr = 0
    DO n = 1,Nhead
        IF (col(n)==0) THEN
            dat(n,i) = NAN_SGL
        ELSE
            ind1 = strfind(line,',',col(n)-1)+1
            IF (col(n)<Ncol) THEN
                ind2 = MAX(ind1,strfind(line,',',col(n))-1)
            ELSE
                ind2 = LEN_TRIM(line)
            END IF
            IF (is_time(n)) THEN
                READ(line(ind1:ind2),'(a)',IOSTAT=tmpstat) timestr
                dat(n,i) = timein(timestr,tform,tzero)
                IF (ISNAN(dat(n,i))) THEN
                    CALL message('error reading time on line '//num2str(i)//' of csv file','(a)')
                    IF (tform==1) CALL message('time format must be "dd/mm/yyyy HH:MM:SS" (or some truncation)','(a)')
                    CALL err(sub,'error reading input file'); RETURN
                END IF
            ELSE
                READ(line(ind1:ind2), *,IOSTAT=tmpstat) dat(n,i)
                IF (tmpstat/=0) ierr = tmpstat
                IF (ierr/=0) THEN
                    CALL message('error reading line '//num2str(i)//' of input file','(a)')
                    CALL err(sub,'error reading input file')
                END IF
            END IF
            
        END IF
    END DO
END DO

! REPORT RESULTS
CALL message(TRIM(tmpstr)//' columns read from input file.','(a\)')
CALL message(' '//num2str(Nline)//' lines successfully read.','(a)')

! CLOSE FILE
CALL closefile(unit)
CALL errchk(sub); IF (errstat) RETURN

END SUBROUTINE read_evo_file
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE bc_update(bc)
! UPDATE EVO MODEL BC OBJECTS
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(BCtyp),ALLOCATABLE,DIMENSION(:),INTENT(INOUT) :: bc
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: i

DO i = 1,SIZE(bc)
    SELECT CASE (bc(i)%typ)
    CASE (1,2,3) ! CSV BCS
        CALL update_bc_csv(bc(i))
    CASE (4) ! SOURCE/SINK BC
        CALL update_bc_sourcesink(bc(i))
    END SELECT
END DO

CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE update_bc_csv(bc)
USE GEN_UTIL, ONLY : interp1
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(BCtyp),INTENT(INOUT) :: bc
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: i, k

IF (bc%loop) THEN
    k = maxloc(bc%time-t,MASK=bc%time-t .LE. 0,DIM=1)
    IF (k==SIZE(bc%time)) THEN
        bc%time = bc%time + t - bc%time(1)
    END IF
END IF

SELECT CASE(bc%typ)
CASE (1,2,3) ! WAVE
    DO i = 1,SIZE(bc%tdat)
        bc%tdat(i) = interp1(bc%time, bc%dat(i,:), t)
    END DO
END SELECT

END SUBROUTINE update_bc_csv
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE update_bc_sourcesink(bc)
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(BCtyp),INTENT(INOUT) :: bc
! LOCAL PARAMETERS
REAL(KIND=DBL) :: convert_time_to_years = 365.25_DBL * 24._DBL * 3600._DBL
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: n, i1, i2
REAL(KIND=SGL) :: rate, vol
REAL(KIND=DBL) :: tstart, tend

DO n = 1,SIZE(bc%obj)
    tstart = REAL(bc%dat(3,n), DBL)
    tend = REAL(bc%dat(4,n), DBL)
    IF (t>=tstart-0.0001_DBL .AND. t<=tend+0.0001_DBL) THEN
        rate = bc%dat(5,n)
        vol = bc%dat(6,n)
        IF (.NOT.ISNAN(rate)) THEN
            bc%tdat(n) = rate
        ELSE IF (.NOT.ISNAN(vol)) THEN
            bc%tdat(n) = vol / REAL((tend - tstart)/convert_time_to_years, SGL)
        ELSE
            bc%tdat(n) = 0.
        END IF
    ELSE
        bc%tdat(n) = 0.
    END IF
END DO

END SUBROUTINE update_bc_sourcesink
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE bc_update
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

END MODULE BCmod
