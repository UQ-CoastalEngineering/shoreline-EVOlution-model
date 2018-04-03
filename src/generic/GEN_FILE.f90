! GEN_FILE - Generic File Handling Procedures Module
!
! Copyright WBM 2007
! Authors: Ian Teakle

MODULE GEN_FILE
!!DEC$ STRICT

INCLUDE 'COMPILER_DIRECTIVES.FI'

! MODULE USE STATEMENTS
USE PRECISION
USE GEN_ERROR
USE LOG
USE GEN_STRING

! MODULE ACCESS STATEMENTS
PRIVATE
PUBLIC :: Nfil
PUBLIC :: fileparts, fullfile
PUBLIC :: openfile, closefile, closeall, csvread, wrtdat, read_matrix_file
PUBLIC :: checkNCStatus, nf90_get_varnames, nf90_get_varid

! MODULE INTERFACE STATEMENTS
INTERFACE csvread
    MODULE PROCEDURE csvread_r
!DEC$ IF (_PRECISION==1)
    MODULE PROCEDURE csvread_d
!DEC$ END IF
END INTERFACE

! MODULE DECLARATIONS
! MAXIMUM NUMBER OF INPUT AND OUTPUT FILES PARAMETER DECLARATION
INTEGER(KIND=ISGL),PARAMETER :: Nfilmax = 100
INTEGER(KIND=ISGL),PARAMETER :: fidstart = 101
! PUBLIC VARIABLE DECLARATIONS
INTEGER(KIND=ISGL),SAVE :: Nfil = 0

! MODULE PROCEDURES
CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PURE SUBROUTINE fileparts(filestr,path,name,ext)
! FILEPARTS RETURNS THE PATH, FILENAME(WITHOUT EXTENSION) AND EXTENSION GIVEN A FILE

! SUBROUTINE DECLARATIONS
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
CHARACTER(LEN=*),INTENT(IN) :: filestr
CHARACTER(LEN=LEN(filestr)),OPTIONAL,INTENT(OUT) :: path
CHARACTER(LEN=LEN(filestr)),OPTIONAL,INTENT(OUT) :: name
CHARACTER(LEN=LEN(filestr)),OPTIONAL,INTENT(OUT) :: ext
! LOCAL VARRIABLES
CHARACTER(LEN=1) :: separator
INTEGER(KIND=ISGL) :: i, j

!DEC$ IF (PLATFORM==1)
separator = '\'
!DEC$ ELSE
separator = '/'
!DEC$ END IF

i = INDEX(filestr,separator,.TRUE.)
j = INDEX(filestr,'.',.TRUE.)
IF ((j/=0) .AND. (i/=0)) THEN
    IF (PRESENT(path)) path = filestr(:i)
    IF (PRESENT(name)) name = filestr(i+1:j-1)
    IF (PRESENT(ext)) ext = filestr(j:)
ELSEIF ((j==0) .AND. (i/=0)) THEN
    IF (PRESENT(path)) path = filestr(:i)
    IF (PRESENT(name)) name = filestr(i+1:)
    IF (PRESENT(ext)) ext = ''
ELSEIF ((j/=0) .AND. (i==0)) THEN
    IF (PRESENT(path)) path = ''
    IF (PRESENT(name)) name = filestr(1:j-1)
    IF (PRESENT(ext)) ext = filestr(j:)
ELSE ! ((j==0) .AND. (i==0)) THEN
    IF (PRESENT(path)) path = ''
    IF (PRESENT(name)) name = filestr(1:)
    IF (PRESENT(ext)) ext = ''
END IF

END SUBROUTINE fileparts
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE openfile(UNIT,FILE,STATUS,ACCESS,ACTION,FORM,RECL,IOSTAT)
! FVWBM ROUTINE TO OPEN FILES

! SUBROUTINE DECLARATIONS
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
INTEGER(KIND=ISGL),INTENT(OUT) :: UNIT
CHARACTER(LEN=*),INTENT(IN) :: FILE
CHARACTER(LEN=15),OPTIONAL,INTENT(IN) :: STATUS
CHARACTER(LEN=15),OPTIONAL,INTENT(IN) :: ACCESS
CHARACTER(LEN=15),OPTIONAL,INTENT(IN) :: ACTION
CHARACTER(LEN=15),OPTIONAL,INTENT(IN) :: FORM
INTEGER(KIND=ISGL),OPTIONAL,INTENT(IN) :: RECL
INTEGER(KIND=ISGL),OPTIONAL,INTENT(OUT) :: IOSTAT
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'openfile'
INTEGER(KIND=ISGL) :: fid
CHARACTER(LEN=LEN(FILE)) :: fil
CHARACTER(LEN=LEN(STATUS)) :: stat
CHARACTER(LEN=LEN(ACCESS)) :: acc
CHARACTER(LEN=LEN(ACTION)) :: act
CHARACTER(LEN=LEN(FORM)) :: for
INTEGER(KIND=ISGL) :: rec
INTEGER(KIND=ISGL) :: iostatus
INTEGER(KIND=ISGL) :: ierr
LOGICAL :: openstat
CHARACTER(LEN=80) :: str

IF (.NOT.PRESENT(STATUS)) THEN
    stat = 'UNKNOWN'
ELSE
    stat = upper(STATUS)
END IF
IF (.NOT.PRESENT(ACCESS)) THEN
    acc = 'SEQUENTIAL'
ELSE
    acc = upper(ACCESS)
END IF
IF (.NOT.PRESENT(ACTION)) THEN
    act = 'READWRITE'
ELSE
    act = upper(ACTION)
END IF
IF (.NOT.PRESENT(FORM)) THEN
    IF (acc=='SEQUENTIAL') THEN
        for = 'FORMATTED'
    ELSE
        for = 'UNFORMATTED'
    END IF
ELSE
    for = upper(FORM)
END IF
IF (.NOT.PRESENT(RECL)) THEN
    IF (for=='FORMATTED') THEN
        rec = 1
    ELSE
        rec = 4
    END IF
ELSE
    rec = RECL
END IF

fil = file
CALL message('Trying to open file: '//TRIM(fil)//' ...','(a\)')

! DETERMINE UNIT NUMBER
! FIND LOWEST AVAILABLE UNIT NUMBER BETWEEN FIDSTART AND FIDSTART+NFILMAX
IF (Nfil+1>fidstart+Nfilmax) THEN
    CALL err(sub,'Maximum number of open files exceeded'); RETURN
ELSE
    Nfil = Nfil + 1
END IF
fid = fidstart
DO
    INQUIRE(fid,OPENED=openstat)
    IF (openstat) THEN
        fid = fid + 1
    ELSE
        EXIT
    END IF
END DO

! TRY TO OPEN FILE
IF ((for=='FORMATTED').AND.(acc=='SEQUENTIAL')) THEN
    OPEN(UNIT=fid,FILE=fil,STATUS=stat,ACCESS=acc,ACTION=act,FORM=for,IOSTAT=ierr)
ELSEIF ((for=='UNFORMATTED').AND.(acc=='SEQUENTIAL')) THEN
    OPEN(UNIT=fid,FILE=fil,STATUS=stat,ACCESS=acc,ACTION=act,FORM=for,IOSTAT=ierr)
ELSEIF ((for=='UNFORMATTED').AND.(acc=='DIRECT')) THEN
    OPEN(UNIT=fid,FILE=fil,STATUS=stat,ACCESS=acc,ACTION=act,FORM=for,RECL=rec,IOSTAT=ierr)
ELSEIF ((for=='BINARY').AND.(acc=='SEQUENTIAL')) THEN
    OPEN(UNIT=fid,FILE=fil,STATUS=stat,ACCESS=acc,ACTION=act,FORM=for,RECL=rec,IOSTAT=ierr)
ELSE
    CALL err(sub,'openfile argument combination not supported'); RETURN
END IF

IF (ierr /= 0) THEN
    IF (.NOT.PRESENT(IOSTAT)) THEN
        CALL message('NOT OK','(a)')
        CALL err(sub,'Unable to open file: '//TRIM(fil)); RETURN
    ELSE
        IOSTAT = ierr
        CALL message('NOT OK','(a)')
        CALL warn('Unable to open file: '//TRIM(fil)); RETURN
    END IF
END IF

UNIT = fid

CALL message('OK. File unit: '//num2str(fid),'(1X,a)')

END SUBROUTINE openfile
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE closefile(UNIT,IOSTAT)
! FVWBM ROUTINE TO CLOSE FILES

! SUBROUTINE USE STATEMENTS
USE GEN_ERROR
USE LOG
! SUBROUTINE DECLARATIONS
IMPLICIT NONE
INTEGER(KIND=ISGL),INTENT(IN) :: UNIT
INTEGER(KIND=ISGL),OPTIONAL,INTENT(OUT) :: IOSTAT
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'closefile'
INTEGER(KIND=ISGL) :: fid
LOGICAL :: openstat
INTEGER(KIND=ISGL) :: ierr
CHARACTER(LEN=80) :: str

fid = UNIT

INQUIRE(fid,OPENED=openstat)
IF (openstat) THEN
    CLOSE(UNIT=fid,IOSTAT=ierr)
    
    IF (ierr /= 0) THEN
        IF (.NOT.PRESENT(IOSTAT)) THEN
            CALL err(sub,'Error encountered closing file unit '//num2str(fid)); RETURN
        ELSE
            IOSTAT=ierr
        END IF
    ELSE
        CALL message('Closing file unit: '//num2str(fid),'(a)')
    END IF
    Nfil = Nfil - 1
ELSE
    IF (.NOT.PRESENT(IOSTAT)) THEN
        CALL err(sub,'File unit '//num2str(fid)//' is not open'); RETURN
    ELSE
        IOSTAT=ierr
    END IF
END IF

END SUBROUTINE closefile
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE closeall()
! FVWBM ROUTINE TO CLOSE ALL OPEN FILES

! SUBROUTINE USE STATEMTNS
USE GEN_ERROR
! SUBROUTINE DECLARATIONS
IMPLICIT NONE
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'closeall'
INTEGER(KIND=ISGL) :: fid, n, tmpstat

DO n = 1,Nfilmax
    fid = fidstart + n - 1
    CALL closefile(fid,IOSTAT=tmpstat)
    CALL errchk(sub); IF (errstat) RETURN
END DO

END SUBROUTINE closeall
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE csvread_r(file, head, dat, time, timeformat, timezero)
! READ DATA FROM A CSV FILE (UNIT).
! HEAD IS A CHARACTER ARRAY OF THE COLUMN HEADINGS WE ARE CHASING.
! DAT IS A REAL ARRAY (:,:) OF THE DATA WE ARE CHASING.
! TIME IS A DOUBLE PRECISION VECTOR (:) OF THE TIME DATA WE ARE CHASING (OPTIONAL).
! TIMEFORMAT AND TIMEZERO DESCRIBE THE TIME FORMAT AND REFERENCE DATE (ALSO OPTIONAL).

! SUBROUTINE USE STATEMENTS
USE GEN_UTIL, ONLY : timein
! SUBROUTINE DECLARATIONS
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
CHARACTER(LEN=*),INTENT(IN) :: file
CHARACTER(LEN=*),DIMENSION(:),INTENT(IN) :: head
REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:,:),INTENT(OUT) :: dat
REAL(KIND=DBL),ALLOCATABLE,DIMENSION(:),OPTIONAL,INTENT(OUT) :: time
INTEGER(KIND=ISGL),OPTIONAL,INTENT(IN) :: timeformat
REAL(KIND=DBL),OPTIONAL,INTENT(IN) :: timezero
! LOCAL PARAMETERS
INTEGER(KIND=ISGL),PARAMETER :: VLSTR = 4000
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'csvread_r'
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
INTEGER(KIND=ISGL) :: tform
REAL(KIND=DBL) :: tzero
INTEGER(KIND=ISGL) :: tmpstat, ierr
INTEGER(KIND=ISGL) :: alloc_stat, dealloc_stat

! GET TIME FORMAT (IF PRESENT)
IF (PRESENT(timeformat)) THEN
    tform = timeformat
ELSE
    tform = 0
END IF

! GET ZERO TIME (IF PRESENT)
IF (PRESENT(timezero)) THEN
    tzero = timezero
ELSE
    tzero = 0._DBL
END IF

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
IF (PRESENT(time)) THEN
    ALLOCATE(time(Nline), STAT=alloc_stat)
    ALLOCATE(dat(Nhead-1,Nline), STAT=alloc_stat)
ELSE
    ALLOCATE(dat(Nhead,Nline), STAT=alloc_stat)
END IF
IF (alloc_stat/=0) THEN
    CALL err(sub,'allocate error'); RETURN
END IF
! GET COLUMN NUMBERS CORRESPONDING TO BC HEADERS
tmpstr = ''
IF (PRESENT(time)) THEN
    col(1) = strmatch(csvhead,upper(head(1)),.FALSE.)
    IF (col(1)==0) THEN
        CALL err(sub,TRIM(upper(head(1)))//' column not found in csv file.')
    ELSE
        tmpstr = TRIM(tmpstr)//TRIM(head(1))//','
    END IF
    DO n = 2,Nhead
        col(n) = strmatch(csvhead,upper(head(n)),.FALSE.)
        IF (col(n)==0) THEN
            CALL warn(TRIM(upper(head(n)))//' column not found in csv file. Assigning a value of 0.')
        ELSE
            tmpstr = TRIM(tmpstr)//TRIM(head(n))//','
        END IF
    END DO
ELSE
    DO n = 1,Nhead
        col(n) = strmatch(csvhead,upper(head(n)),.FALSE.)
        IF (col(n)==0) THEN
            CALL warn(TRIM(upper(head(n)))//' column not found in csv file. Assigning a value of 0.')
        ELSE
            tmpstr = TRIM(tmpstr)//TRIM(head(n))//','
        END IF
    END DO
END IF
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
        CALL err(sub,'error reading csv file'); EXIT
    END IF
    ierr = 0
    IF (PRESENT(time)) THEN
        ind1 = strfind(line,',',col(1)-1)+1
        IF (col(1)<Ncol) THEN
            ind2 = MAX(ind1,strfind(line,',',col(1))-1)
        ELSE
            ind2 = LEN_TRIM(line)
        END IF
        READ(line(ind1:ind2),'(a)',IOSTAT=tmpstat) timestr
        time(i) = timein(timestr,tform,tzero)
        IF (ISNAN(time(i))) THEN
            CALL message('error reading time on line '//num2str(i)//' of csv file','(a)')
            IF (tform==1) CALL message('time format must be "dd/mm/yyyy HH:MM:SS" (or some truncation)','(a)')
            CALL err(sub,'error reading csv file'); RETURN
        END IF
        DO n = 2,Nhead
            IF (col(n)==0) THEN
                dat(n-1,i) = 0.
            ELSE
                ind1 = strfind(line,',',col(n)-1)+1
                IF (col(n)<Ncol) THEN
                    ind2 = MAX(ind1,strfind(line,',',col(n))-1)
                ELSE
                    ind2 = LEN_TRIM(line)
                END IF
                READ(line(ind1:ind2),*,IOSTAT=tmpstat) dat(n-1,i)
                IF (tmpstat/=0) ierr = tmpstat
            END IF
        END DO
    ELSE
        DO n = 1,Nhead
            IF (col(n)==0) THEN
                dat(n,i) = 0.
            ELSE
                ind1 = strfind(line,',',col(n)-1)+1
                IF (col(n)<Ncol) THEN
                    ind2 = MAX(ind1,strfind(line,',',col(n))-1)
                ELSE
                    ind2 = LEN_TRIM(line)
                END IF
                READ(line(ind1:ind2), *,IOSTAT=tmpstat) dat(n,i)
                IF (tmpstat/=0) ierr = tmpstat
            END IF
        END DO
    END IF
    IF (ierr/=0) THEN
        CALL message('error reading line '//num2str(i)//' of csv file','(a)')
        CALL err(sub,'error reading csv file')
    END IF
END DO

! REPORT RESULTS
CALL message(TRIM(tmpstr)//' columns read from csvfile.','(a\)')
CALL message(' '//num2str(Nline)//' lines successfully read.','(a)')

! CLOSE FILE
CALL closefile(unit)
CALL errchk(sub); IF (errstat) RETURN

END SUBROUTINE csvread_r
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DEC$ IF (_PRECISION==1)
SUBROUTINE csvread_d(file, head, dat, time, timeformat, timezero)
! READ DATA FROM A CSV FILE (UNIT).
! HEAD IS A CHARACTER ARRAY OF THE COLUMN HEADINGS WE ARE CHASING.
! DAT IS A REAL ARRAY (:,:) OF THE DATA WE ARE CHASING.
! TIME IS A DOUBLE PRECISION VECTOR (:) OF THE TIME DATA WE ARE CHASING (OPTIONAL).
! TIMEFORMAT AND TIMEZERO DESCRIBE THE TIME FORMAT AND REFERENCE DATE (ALSO OPTIONAL).

! SUBROUTINE USE STATEMENTS
USE GEN_UTIL, ONLY : timein
! SUBROUTINE DECLARATIONS
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
CHARACTER(LEN=*),INTENT(IN) :: file
CHARACTER(LEN=*),DIMENSION(:),INTENT(IN) :: head
REAL(KIND=DBL),ALLOCATABLE,DIMENSION(:,:),INTENT(OUT) :: dat
REAL(KIND=DBL),ALLOCATABLE,DIMENSION(:),OPTIONAL,INTENT(OUT) :: time
INTEGER(KIND=ISGL),OPTIONAL,INTENT(IN) :: timeformat
REAL(KIND=DBL),OPTIONAL,INTENT(IN) :: timezero
! LOCAL PARAMETERS
INTEGER(KIND=ISGL),PARAMETER :: VLSTR = 4000
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'csvread_d'
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
INTEGER(KIND=ISGL) :: tform
REAL(KIND=DBL) :: tzero
INTEGER(KIND=ISGL) :: tmpstat, ierr
INTEGER(KIND=ISGL) :: alloc_stat, dealloc_stat

! GET TIME FORMAT (IF PRESENT)
IF (PRESENT(timeformat)) THEN
    tform = timeformat
ELSE
    tform = 0
END IF

! GET ZERO TIME (IF PRESENT)
IF (PRESENT(timezero)) THEN
    tzero = timezero
ELSE
    tzero = 0._DBL
END IF

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
IF (PRESENT(time)) THEN
    ALLOCATE(time(Nline), STAT=alloc_stat)
    ALLOCATE(dat(Nhead-1,Nline), STAT=alloc_stat)
ELSE
    ALLOCATE(dat(Nhead,Nline), STAT=alloc_stat)
END IF
IF (alloc_stat/=0) THEN
    CALL err(sub,'allocate error'); RETURN
END IF
! GET COLUMN NUMBERS CORRESPONDING TO BC HEADERS
tmpstr = ''
IF (PRESENT(time)) THEN
    col(1) = strmatch(csvhead,upper(head(1)),.FALSE.)
    IF (col(1)==0) THEN
        CALL err(sub,TRIM(upper(head(1)))//' column not found in csv file.')
    ELSE
        tmpstr = TRIM(tmpstr)//TRIM(head(1))//','
    END IF
    DO n = 2,Nhead
        col(n) = strmatch(csvhead,upper(head(n)),.FALSE.)
        IF (col(n)==0) THEN
            CALL warn(TRIM(upper(head(n)))//' column not found in csv file. Assigning a value of 0.')
        ELSE
            tmpstr = TRIM(tmpstr)//TRIM(head(n))//','
        END IF
    END DO
ELSE
    DO n = 1,Nhead
        col(n) = strmatch(csvhead,upper(head(n)),.FALSE.)
        IF (col(n)==0) THEN
            CALL warn(TRIM(upper(head(n)))//' column not found in csv file. Assigning a value of 0.')
        ELSE
            tmpstr = TRIM(tmpstr)//TRIM(head(n))//','
        END IF
    END DO
END IF
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
        CALL err(sub,'error reading csv file'); EXIT
    END IF
    ierr = 0
    IF (PRESENT(time)) THEN
        ind1 = strfind(line,',',col(1)-1)+1
        IF (col(1)<Ncol) THEN
            ind2 = MAX(ind1,strfind(line,',',col(1))-1)
        ELSE
            ind2 = LEN_TRIM(line)
        END IF
        READ(line(ind1:ind2),'(a)',IOSTAT=tmpstat) timestr
        time(i) = timein(timestr,tform,tzero)
        IF (ISNAN(time(i))) THEN
            CALL message('error reading time on line '//num2str(i)//' of csv file','(a)')
            IF (tform==1) CALL message('time format must be "dd/mm/yyyy HH:MM:SS" (or some truncation)','(a)')
            CALL err(sub,'error reading csv file'); RETURN
        END IF
        DO n = 2,Nhead
            IF (col(n)==0) THEN
                dat(n-1,i) = 0.
            ELSE
                ind1 = strfind(line,',',col(n)-1)+1
                IF (col(n)<Ncol) THEN
                    ind2 = MAX(ind1,strfind(line,',',col(n))-1)
                ELSE
                    ind2 = LEN_TRIM(line)
                END IF
                READ(line(ind1:ind2),*,IOSTAT=tmpstat) dat(n-1,i)
                IF (tmpstat/=0) ierr = tmpstat
            END IF
        END DO
    ELSE
        DO n = 1,Nhead
            IF (col(n)==0) THEN
                dat(n,i) = 0.
            ELSE
                ind1 = strfind(line,',',col(n)-1)+1
                IF (col(n)<Ncol) THEN
                    ind2 = MAX(ind1,strfind(line,',',col(n))-1)
                ELSE
                    ind2 = LEN_TRIM(line)
                END IF
                READ(line(ind1:ind2), *,IOSTAT=tmpstat) dat(n,i)
                IF (tmpstat/=0) ierr = tmpstat
            END IF
        END DO
    END IF
    IF (ierr/=0) THEN
        CALL message('error reading line '//num2str(i)//' of csv file','(a)')
        CALL err(sub,'error reading csv file')
    END IF
END DO

! REPORT RESULTS
CALL message(TRIM(tmpstr)//' columns read from csvfile.','(a\)')
CALL message(' '//num2str(Nline)//' lines successfully read.','(a)')

! CLOSE FILE
CALL closefile(unit)
CALL errchk(sub); IF (errstat) RETURN

END SUBROUTINE csvread_d
!DEC$ END IF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE read_matrix_file(x,y,dat,file)
!*******************************************************************************
! READS IN MATRIX DATA FROM 'file' (path to a csv file)
! DATA STARTS ON FIRST CSV LINE WITH MORE THAN 3 VALUES ON THE LINE
! FIRST VALUE ON THE FIRST LINE IS A SCALING FACTOR, IF BLANK, ASSUMES 1.0
! REMAINING VALUES ARE THE X INDICES
! FIRST VALUE ON FOLLOWING LINES ARE THE Y INDICES, AND REMAINING VALUES ON 
! THOSE LINES ARE THE MATRIX DATA, REFERENCED BY X & Y INDICES
!*******************************************************************************
! SUBROUTINE USE STATEMENTS

! SUBROUTINE DECLARATIONS
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:),INTENT(OUT) :: x
REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:),INTENT(OUT) :: y
REAL(KIND=SGL),ALLOCATABLE,DIMENSION(:,:),INTENT(OUT) :: dat
CHARACTER(LEN=*),INTENT(IN) :: file

! LOCAL PARAMETERS
INTEGER(KIND=ISGL),PARAMETER :: VLSTR = 4000
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'read_matrix_file'
INTEGER(KIND=ISGL) :: unit
INTEGER(KIND=ISGL) :: Ncol, validy
INTEGER(KIND=ISGL) :: Nline,validx
INTEGER(KIND=ISGL) :: alloc_stat
CHARACTER(LEN=VLSTR) :: line
INTEGER(KIND=ISGL) :: ierr,i,j
CHARACTER(LEN=LSTR) :: tmpstr
CHARACTER(LEN=VLSTR):: token
CHARACTER(LEN=VLSTR) :: remain
INTEGER(KIND=ISGL) :: firstline
REAL(KIND=SGL) :: scale
LOGICAL(KIND=ISGL) :: firstlinefound

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

Nline = 0
DO
    READ(unit,'(a)', IOSTAT=ierr) line
    IF (ierr/=0) THEN
        Nline = Nline+1
        CALL message('error reading line '//num2str(Nline)//' of csv file','(a)')
        CALL err(sub,'error reading csv file')
        EXIT
    END IF
    tmpstr = strrep(line,',','')
    Nline = Nline + 1
    IF (tmpstr=='') EXIT
END DO
! REWIND FILE
REWIND(unit)
firstlinefound = .false.
firstline = 0
DO WHILE (not(firstlinefound))
    firstline = firstline + 1
    firstlinefound = .true.
    READ(unit,'(a)', IOSTAT=ierr) line
    remain = line
    DO i = 1,3  ! line of x indices is first row with 3 items 
      line = remain
      CALL parse(line,',',token,remain)
      IF (token=='') THEN
        firstlinefound = .false.        
      END IF
    END DO
END DO
! HOW MANY VALID VALUES ON THE  FIRST LINE?
validx = 2
x_valids: DO 
  line = remain
  CALL parse(line,',',token,remain)
  IF (trim(token).ne.'') THEN
    validx = validx+1
  END IF
  IF (trim(remain).eq.'') EXIT x_valids
END DO x_valids
! HOW MANY VALID LINES?
validy = 0
y_valids: DO 
  READ(unit,'(a)', IOSTAT=ierr) line
  IF (ierr.lt.0) EXIT y_valids ! END OF THE LINE
  CALL parse(line,',',token,remain)
  IF (trim(token).ne.'') THEN
    validy = validy+1
  END IF  
END DO y_valids
! REWIND FILE
REWIND(unit)    
! ALLOCATE SPACE
ALLOCATE(y(validy),STAT=alloc_stat)
ALLOCATE(x(validx),STAT=alloc_stat)
ALLOCATE(dat(validx,validy),STAT=alloc_stat)
IF (alloc_stat/=0) THEN
    CALL err(sub,'allocate error'); RETURN
END IF
y = -99999.99
x = -99999.99
dat = -9999.99
! READ DISPOSABLE LINES
DO i = 1,firstline
  READ(unit,'(a)', IOSTAT=ierr) line
END DO
! Handles first Line with x values
CALL parse(line,',',token,remain)
READ(token,*) scale 
i = 0
DO i = 1,validx
  line = remain
  CALL parse(line,',',token,remain)
  READ(token,*)x(i) 
END DO 

DO i = 1,validy
  READ(unit,'(a)', IOSTAT=ierr) line
  CALL parse(line,',',token,remain)  
  READ(token,*)y(i)   
  DO j = 1,validx
    line = remain
    CALL parse(line,',',token,remain)  
    READ(token,*)dat(j,i)
  END DO
END DO
dat = dat * scale   ! DAT * SCALE IS A TOTAL VALUE FOR THE WHOLE STRUCTURE
! CLOSE FILE
CALL closefile(unit)
CALL errchk(sub); IF (errstat) RETURN

END SUBROUTINE read_matrix_file
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE wrtdat(fid,init,name,t,numcells,cellstat,dat)
! WRITE SMS BINARY DAT FILE

! SUBROUTINE USE STATEMENTS
USE GEN_STRING, ONLY : pad, num2str
! SUBROUTINE DECLARATIONS
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
INTEGER(KIND=ISGL),INTENT(IN) :: fid
LOGICAL,INTENT(INOUT) :: init
CHARACTER(LEN=40),INTENT(IN) :: name
REAL(KIND=DBL),INTENT(IN) :: t
INTEGER(KIND=ISGL),INTENT(IN) :: numcells
LOGICAL,DIMENSION(numcells),INTENT(IN) :: cellstat
REAL(KIND=SGL),DIMENSION(:,:),INTENT(IN) :: dat
! LOCAL PARAMETERS
INTEGER(KIND=4),PARAMETER :: I32 = 4
INTEGER(KIND=4),PARAMETER :: SFLG = 1
INTEGER(KIND=4),PARAMETER :: SFLT = 4
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'fvout_wrtdat'
INTEGER(KIND=4) :: numdata
LOGICAL :: vec
INTEGER(KIND=4) :: i
INTEGER(KIND=SFLG),DIMENSION(numcells) :: tmpstat
INTEGER(KIND=ISGL) :: ierr

IF (SIZE(dat,1)==1) THEN
    vec = .FALSE.
ELSEIF (SIZE(dat,1)==2) THEN
    vec = .TRUE.
ELSE
    CALL err(sub,'SIZE(dat,1) must be 1 or 2'); RETURN
END IF

numdata = SIZE(dat,2)
tmpstat = cellstat

! IF NOT INITIALISED WRITE HEADER INFORMATION
IF (.NOT.init) THEN
    init = .TRUE.
    ! VERSION
    WRITE(fid,IOSTAT=ierr) 3000_I32
    ! OBJTYPE
    WRITE(fid,IOSTAT=ierr) 100_I32, 3_I32
    ! SFLT
    WRITE(fid,IOSTAT=ierr) 110_I32, SFLT
    ! SFLG
    WRITE(fid,IOSTAT=ierr) 120_I32, SFLG
    ! TIME UNITS
    WRITE(fid,IOSTAT=ierr) 250_I32, 0_I32
    ! BEGSCL / BEGVEC
    IF (.NOT.vec) THEN
        WRITE(fid,IOSTAT=ierr) 130_I32
    ELSE
        WRITE(fid,IOSTAT=ierr) 140_I32
    END IF
    ! NUMDATA
    WRITE(fid,IOSTAT=ierr) 170_I32, numdata
    ! NUMCELLS
    WRITE(fid,IOSTAT=ierr) 180_I32, numcells
    ! NAME
    WRITE(fid,IOSTAT=ierr) 190_I32, name
END IF

! TS
WRITE(fid,IOSTAT=ierr) 200_I32
! ISTAT
WRITE(fid,IOSTAT=ierr) 1_SFLG
! TIME
WRITE(fid,IOSTAT=ierr) REAL(t/3600._DBL,KIND=SFLT)
! STAT
WRITE(fid,IOSTAT=ierr) tmpstat
! VAL
WRITE(fid,IOSTAT=ierr) REAL(dat,KIND=SFLT)

IF (ierr/=0) THEN
    CALL err(sub,'error writing to file unit'//num2str(fid)); RETURN
END IF

END SUBROUTINE wrtdat
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DEC$ IF (INCL_NETCDF==1)
SUBROUTINE CheckNCStatus(ncStatus,doHaltOnErr,ncCommand)
! SUBROUTINE USE STATEMENTS
USE NETCDF, ONLY : nf90_noerr, nf90_strerror
! SUBROUTINE DECLARATIONS
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
INTEGER,INTENT(IN) :: ncStatus      !status code from netCDF
LOGICAL,INTENT(IN) :: doHaltOnErr   !halt on error - T or F?
CHARACTER(LEN=*),INTENT(IN) :: ncCommand     !the netCDF command
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'CheckNCStatus'

IF (ncStatus == nf90_noerr) THEN
  RETURN
END IF

IF (doHaltOnErr) THEN
    CALL err(sub,'Error invoking '//TRIM(ncCommand)//&
    '. Netcdf status value is '//num2str(ncStatus)//&
    '. '//TRIM(nf90_strerror(ncStatus))//'.')
    RETURN
ELSE
    CALL warn('Error invoking '//TRIM(ncCommand)//&
    '. Netcdf status value is '//num2str(ncStatus)//&
    '. '//TRIM(nf90_strerror(ncStatus))//'.')
    RETURN
END IF

END SUBROUTINE CheckNCStatus
!DEC$ END IF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DEC$ IF (INCL_NETCDF==1)
FUNCTION nf90_get_varnames(ncid,varnames) RESULT(ncStatus)
! FUNCTION USE STATEMENTS
USE NETCDF, ONLY : nf90_inquire, nf90_inquire_variable, nf90_noerr
! FUNCTION DECLARATIONS
IMPLICIT NONE
! FUNCTION ARGUMENTS
INTEGER,INTENT(IN) :: ncid
CHARACTER(LEN=MSTR),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: varnames
! FUNCTION RESULT
INTEGER :: ncStatus
! LOCAL VARIABLES
INTEGER :: Nvar
INTEGER :: alloc_stat
INTEGER :: i

ncStatus = nf90_inquire(NCID = ncid, NVARIABLES = Nvar)
IF (ncStatus /= nf90_noerr) RETURN
ALLOCATE(varnames(Nvar), STAT=alloc_stat)
IF (alloc_stat/=0) RETURN
DO i = 1 , Nvar
    ncStatus = nf90_inquire_variable(NCID = ncid, VARID = i, NAME = varnames(i))
    IF (ncStatus /= nf90_noerr) RETURN
END DO

END FUNCTION
!DEC$ END IF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DEC$ IF (INCL_NETCDF==1)
FUNCTION nf90_get_varid(ncid,varname,varid) RESULT(ncStatus)
! FUNCTION USE STATEMENTS
USE NETCDF, ONLY : nf90_inquire, nf90_inquire_variable, nf90_noerr
USE GEN_STRING, ONLY : upper
! FUNCTION DECLARATIONS
IMPLICIT NONE
! FUNCTION ARGUMENTS
INTEGER,INTENT(IN) :: ncid
CHARACTER(LEN=*),INTENT(IN) :: varname
INTEGER,INTENT(OUT) :: varid
! FUNCTION RESULT
INTEGER :: ncStatus
! LOCAL VARIABLES
INTEGER :: Nvar
CHARACTER(LEN=MSTR) :: ncvarname
INTEGER :: i

varid = 0
ncStatus = nf90_inquire(NCID = ncid, NVARIABLES = Nvar)
IF (ncStatus /= nf90_noerr) RETURN
DO i = 1 , Nvar
    ncStatus = nf90_inquire_variable(NCID = ncid, VARID = i, NAME = ncvarname)
    IF (ncStatus /= nf90_noerr) RETURN
    IF (upper(varname)==upper(ncvarname)) THEN
        varid = i; EXIT
    END IF
END DO

END FUNCTION
!DEC$ END IF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE GEN_FILE