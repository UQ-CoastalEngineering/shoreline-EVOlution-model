! CTRL - Control Class Module
!
! Copyright WBM 2007
! Authors: Ian Teakle

MODULE EVOCTRL

INCLUDE 'COMPILER_DIRECTIVES.FI'

! MODULE USE STATEMENTS
USE PRECISION
USE GEN_ERROR
USE globals, ONLY : global_ctrl_typ
USE BCmod, ONLY : BcCtrlTyp
USE LSmod, ONLY : LSparamtyp
USE OUTmod, ONLY : OutCtrlTyp
! MODULE DECLARATIONS
IMPLICIT NONE

! MODULE ACCESS STATEMENTS
PRIVATE
PUBLIC :: ctrl_typ
PUBLIC :: construct, destruct
PUBLIC :: ctrl

! MODULE INTERFACE STATEMENTS
INTERFACE construct
    MODULE PROCEDURE ctrl_construct
END INTERFACE
INTERFACE destruct
    MODULE PROCEDURE ctrl_destruct
END INTERFACE

! MODULE TYPES

! MODEL CONTROL TYPE
TYPE,EXTENDS(global_ctrl_typ) :: ctrl_typ
    CHARACTER(LEN=LSTR) :: evc
    INTEGER(KIND=ISGL) :: modtyp
    REAL(KIND=DBL) :: tend
    REAL(KIND=SGL) :: dt
    REAL(KIND=SGL) :: dt_dsp
    REAL(KIND=SGL) :: dt_rst
    LOGICAL :: reset_t
    CHARACTER(LEN=LSTR) :: rstfil
    CHARACTER(LEN=LSTR) :: grdfil
    CHARACTER(LEN=LSTR) :: groynfil
    CHARACTER(LEN=LSTR) :: swallfil
    CHARACTER(LEN=LSTR) :: xparfil
    CHARACTER(LEN=LSTR) :: WvTransFil
    CHARACTER(LEN=LSTR) :: initcondfil
    TYPE(LSparamtyp) :: ls_param
    INTEGER(KIND=ISGL) :: nbc
    TYPE(BcCtrlTyp),ALLOCATABLE,DIMENSION(:) :: bc
    INTEGER(KIND=ISGL) :: nout
    CHARACTER(LEN=LSTR) :: outdir
    TYPE(OutCtrlTyp),ALLOCATABLE,DIMENSION(:) :: out
END TYPE

! MODULE PARAMETERS
INTEGER(KIND=ISGL),PARAMETER :: Nfidmax = 5          ! MAXIMUM NUMBER OF INCLUDE FILES
INTEGER(KIND=ISGL),PARAMETER :: Nlerrmax = 10        ! MAXIMUM NUMBER OF FILE READ ERRORS
INTEGER(KIND=ISGL),PARAMETER :: Noutmax = 100        ! MAXIMUM NUMBER OF OUTPUT STATEMENTS

! MODULE OBJECT DECLARATIONS
TYPE(ctrl_typ) :: ctrl

! MODULE PROCEDURES
CONTAINS

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE ctrl_construct(ctrl,evc)
! FVCTRL CONSTRUCTOR SUBROUTINE
! OPEN AND READ SHC FILE IN ORDER TO CHECK INPUT AND ALLOCATE DYNAMIC MEMORY

! SUBROUTINE USE STATEMENTS
USE LOG
USE GEN_STRING
USE GEN_FILE, ONLY : openfile, closefile, fileparts, fullfile
USE GEN_UTIL, ONLY : timein, time2str

! SUBROUTINE DECLARATIONS
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(ctrl_typ),INTENT(OUT) :: ctrl
CHARACTER(LEN=*),INTENT(IN) :: evc
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'modctrl_construct'
INTEGER(KIND=ISGL) :: Nfid
INTEGER(KIND=ISGL),DIMENSION(Nfidmax) :: fid, Nline
CHARACTER(LEN=LSTR) :: line
CHARACTER(LEN=LSTR) :: token, remain, expecting
CHARACTER(LEN=LSTR) :: tmpstr
CHARACTER(LEN=19) :: timestr
CHARACTER(LEN=LSTR) :: errstr
LOGICAL :: linestat
INTEGER(KIND=ISGL) :: Nbc, Nout
INTEGER(KIND=ISGL) :: ierr
INTEGER(KIND=ISGL) :: Nlerr
INTEGER(KIND=ISGL) :: tmpstat, alloc_stat
INTEGER(KIND=ISGL) :: i,j
INTEGER(KIND=ISGL) :: itmp, jtmp
LOGICAL :: ltmp
REAL(KIND=SGL) :: rtmp
LOGICAL :: in_bc, in_output

CALL message('Attempting to process input control file:' ,'(a)')

! OPEN EVC
Nfid = 1
Nbc = 0
Nout = 0
fid = 0
CALL openfile(UNIT=fid(Nfid),FILE=evc,STATUS=pad('OLD',15),ACTION=pad('READ',15))
CALL errchk(sub); IF (errstat) RETURN

! HAVE A QUICK LOOK...
DO
    READ(fid(Nfid),'(a)',IOSTAT=ierr) line
    IF (ierr /= 0) THEN
        IF (Nfid>1) THEN
            CALL closefile(UNIT=fid(Nfid))
            CALL errchk(sub); IF (errstat) RETURN
        ELSE
            REWIND(UNIT=fid(Nfid),IOSTAT=ierr)
            IF (ierr /= 0) THEN
                CALL err(sub,'rewind file failed')
            END IF
        END IF
        IF (Nfid>1) fid(Nfid) = 0
        Nline(Nfid) = 0
        Nfid = Nfid - 1
        IF (Nfid==0) EXIT
    END IF
    Nline(Nfid) = Nline(Nfid) + 1
    line = ADJUSTL(line)
    line = strrep(line,CHAR(9),'')
    IF (iscomment(line)) CYCLE
    line = removecomments(line)
!DEC$ IF (PLATFORM==2)
    line = strrep(line,'\','/')
!DEC$ END IF
    CALL parse(line,'==',token,remain)
    token = strrep(token,' ','')
    token = upper(token)
    SELECT CASE (token)
        CASE ('INCLUDE')
            Nfid = Nfid + 1
            IF (Nfid>Nfidmax) THEN
                CALL err(sub,'Number of include file nests is greater than maximum allowed, '//num2str(Nfidmax)//'.')
                RETURN
            END IF
            CALL openfile(UNIT=fid(Nfid),FILE=strtrim(remain),STATUS=pad('OLD',15),ACTION=pad('READ',15))
            CALL errchk(sub); IF (errstat) RETURN
        CASE ('BC')
            Nbc = Nbc + 1
        CASE ('OUTPUT')
            Nout = Nout + 1
    END SELECT
END DO

! ALLOCATE MEMORY
alloc_stat = 0
ALLOCATE(ctrl%bc(Nbc), STAT=tmpstat); IF (tmpstat/=0) alloc_stat=tmpstat
ALLOCATE(ctrl%out(Nout), STAT=tmpstat); IF (tmpstat/=0) alloc_stat=tmpstat
IF (alloc_stat/=0) THEN
    CALL err(sub,'allocate error'); RETURN
END IF

! SET GLOBAL COMPONENT VALUES
ctrl%evc = evc(:INDEX(evc,'.',.TRUE.)-1)
ctrl%do_xs_update = .TRUE.
ctrl%do_ls_update = .TRUE.
ctrl%tform = 0 ! DECIMAL HOURS FORMAT
ctrl%tzero = 0._DBL
ctrl%tstart = 0._DBL
ctrl%rhow = 1025.
ctrl%rhos = 2650.
ctrl%g = 9.81
ctrl%porosity = 0.4
ctrl%morfac = 1.0
ctrl%breaker_index = 0.78

! SET REMAINING DEFAULT VALUES
ctrl%modtyp = 0
ctrl%tend = 0._DBL
ctrl%dt = 0.
ctrl%dt_dsp = 86400.
ctrl%dt_rst = HUGE(0.)
ctrl%reset_t = .FALSE.
ctrl%rstfil = ''
ctrl%grdfil = ''
ctrl%groynfil = ''
ctrl%swallfil = ''
ctrl%WvTransFil = ''
ctrl%ls_param%K1 = 0.77
ctrl%nbc = Nbc
ctrl%bc(1:Nbc)%typnam = ''
ctrl%bc(1:Nbc)%fil = ''
ctrl%bc(1:Nbc)%loop = .FALSE.
ctrl%outdir = ''
ctrl%out(1:Nout)%typnam = ''
ctrl%out(1:Nout)%fil = ''
ctrl%out(1:Nout)%info = ''
ctrl%out(1:Nout)%tstart = -HUGE(0._SGL)
ctrl%out(1:Nout)%tfinal = HUGE(0._SGL)
ctrl%out(1:Nout)%dt = 0.

! NOW LOOP THROUGH LINES PROPERLY
Nfid = 1
Nline = 0
Nlerr = 0
Nbc = 0
in_bc = .FALSE.
Nout = 0
in_output = .FALSE.
CALL message('Start echoing input file:','(a)')
DO
    linestat = .TRUE.
    READ(fid(Nfid),'(a)',IOSTAT=ierr) line
    IF (ierr /= 0) THEN
        CALL closefile(UNIT=fid(Nfid))
        CALL errchk(sub); IF (errstat) RETURN
        IF (Nfid>1) fid(Nfid) = 0
        Nline(Nfid) = 0
        Nfid = Nfid - 1
        IF (Nfid>0) THEN
            CYCLE
        ELSE
            EXIT
        END IF
    END IF
    Nline(Nfid) = Nline(Nfid) + 1
    line = ADJUSTL(line)
    line = strrep(line,CHAR(9),'')
    IF (iscomment(line)) CYCLE
    line = removecomments(line)
!DEC$ IF (PLATFORM==2)
    line = strrep(line,'\','/')
!DEC$ END IF
    CALL parse(line,'==',token,remain)
    token = strrep(token,' ','')
    token = upper(token)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! SELECT APPROPRIATE INPUT FLAG CASE AND CONTINUE PARSEING ACCORDINGLY!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    IF (.NOT.in_bc .AND. .NOT.in_output) THEN
        SELECT CASE (token)
        
        CASE('LOGDIR')
        
        CASE ('INCLUDE')
            Nfid = Nfid + 1
            CALL openfile(UNIT=fid(Nfid),FILE=strtrim(remain),STATUS=pad('OLD',15),ACTION=pad('READ',15))
            CALL errchk(sub); IF (errstat) RETURN
            
        CASE ('MODELTYPE')
            tmpstr = upper(strtrim(remain))
            SELECT CASE(tmpstr)
            CASE ('SIMPLE')
                ctrl%modtyp = 0
                CALL echo('MODEL TYPE','SIMPLE')
            CASE ('HUXLEY')
                ctrl%modtyp = 1
                CALL echo('MODEL TYPE','HUXLEY')
            CASE DEFAULT
                CALL error_on_line('Unrecognised model type specified on line')
            END SELECT
            
        CASE ('DOXSHOREUPDATE')
            READ(remain, *, IOSTAT=ierr) ctrl%do_xs_update
            IF (ierr/=0) CALL error_on_line('Error reading do xshore update status on line')
            IF (ctrl%do_xs_update) THEN
                CALL echo('DO XSHORE UPDATE','.TRUE.')
            ELSE
                CALL echo('DO XSHORE UPDATE','.FALSE.')
            END IF
            
        CASE ('DOLONGSHORETRANSPORT')
            READ(remain, *, IOSTAT=ierr) ctrl%do_ls_update
            IF (ierr/=0) CALL error_on_line('Error reading do longshore transport status on line')
            IF (ctrl%do_ls_update) THEN
                CALL echo('DO LONGSHORE TRANSPORT','.TRUE.')
            ELSE
                CALL echo('DO LONGSHORE TRANSPORT','.FALSE.')
            END IF
            
        CASE ('MORFAC')
            READ(remain, *, IOSTAT=ierr) ctrl%morfac
            IF (ierr/=0) CALL error_on_line('Error reading morfac on line')
            CALL echo('MORFAC',strtrim(num2str(ctrl%morfac)))
        
        CASE ('BREAKERINDEX')
            READ(remain, *, IOSTAT=ierr) ctrl%breaker_index
            IF (ierr/=0) CALL error_on_line('Error reading breaker index on line')
            CALL echo('BREAKER INDEX',strtrim(num2str(ctrl%breaker_index)))
            
        CASE ('TIMEFORMAT')
            tmpstr = strtrim(remain)
            IF (ierr/=0) CALL error_on_line('Error reading time format on line')
            SELECT CASE (UPPER(tmpstr))
                CASE ('HOURS')
                    ctrl%tform = 0
                    CALL echo('TIME FORMAT','HOURS')
                CASE ('ISODATE')
                    ctrl%tform = 1
                    ctrl%tzero = timein('01/01/1990 00:00:00',ctrl%tform)
                    CALL echo('TIME FORMAT','ISO DATE')
                CASE DEFAULT
                    CALL error_on_line('Error, unexpected time format on line')
            END SELECT
            
        CASE ('REFERENCETIME')
            timestr = strtrim(remain)
            IF (ierr/=0) CALL error_on_line('Error reading reference time on line')
            ctrl%tzero = timein(timestr,ctrl%tform)
            CALL echo('REFERENCE TIME', time2str(ctrl%tzero,ctrl%tform))
            
        CASE ('STARTTIME')
            timestr = strtrim(remain)
            IF (ierr/=0) CALL error_on_line('Error reading start time on line')
            ctrl%tstart = timein(timestr,ctrl%tform,ctrl%tzero)
            IF (ISNAN(ctrl%tstart)) CALL error_on_line('error reading start time on line')
            CALL echo('START TIME', time2str(ctrl%tstart,ctrl%tform,ctrl%tzero))
            
        CASE ('ENDTIME')
            timestr = strtrim(remain)
            IF (ierr/=0) CALL error_on_line('Error reading end time on line')
            ctrl%tend = timein(timestr,ctrl%tform,ctrl%tzero)
            IF (ISNAN(ctrl%tend)) CALL error_on_line('error reading end time on line')
            CALL echo('END TIME', time2str(ctrl%tend,ctrl%tform,ctrl%tzero))
            
        CASE ('TIMESTEP')
            READ(remain, *, IOSTAT=ierr) ctrl%dt
            IF (ierr/=0) CALL error_on_line('Error reading timestep on line')
            CALL echo('TIMESTEP', num2str(ctrl%dt))
            
        CASE ('WRITERESTARTDT')
            READ(remain, *, IOSTAT=ierr) ctrl%dt_rst
            ctrl%dt_rst = ctrl%dt_rst*86400.
            IF (ierr/=0) CALL error_on_line('Error reading restart file timestep on line')
            CALL echo('WRITE RESTART DT', num2str(ctrl%dt_rst/86400.))
            
        CASE ('RESTARTFILE')
            ctrl%rstfil = TRIM(remain)
            CALL echo('RESTART FILE',ctrl%rstfil)
            
        CASE('RESETTIME')
            READ(remain, *, IOSTAT=ierr) itmp
            IF (ierr/=0) CALL error_on_line('Error reading reset time switch on line')
            IF (itmp==1) THEN
                ctrl%reset_t = .TRUE.
                CALL echo('RESET TIME','.TRUE.')
            ELSEIF (itmp==0) THEN
                ctrl%reset_t = .FALSE.
                CALL echo('RESET TIME','.FALSE.')
            ELSE
                CALL error_on_line('Error reading reset time switch on line')
            END IF
            
        CASE ('GRIDFILE')
            ctrl%grdfil = TRIM(remain)
            CALL echo('GRID FILE',ctrl%grdfil)
            
        CASE ('GROYNEDEFINITIONFILE')
            ctrl%groynfil = TRIM(remain)
            CALL echo('GROYNE DEFINITION FILE',ctrl%groynfil)
            
        CASE ('SEAWALLDEFINITIONFILE')
            ctrl%swallfil = TRIM(remain)
            CALL echo('SEAWALL DEFINITION FILE',ctrl%swallfil)
            
        CASE ('XSECTPARAMETERFILE')
            ctrl%xparfil = TRIM(remain)
            CALL echo('XSECT PARAMETER FILE',ctrl%xparfil)
            
        CASE ('WAVETRANSFORMATIONFILE')
            ctrl%WvTransFil = TRIM(remain)
            CALL echo('WAVE TRANSFORMATION FILE',ctrl%WvTransFil)
            
        CASE ('INITIALCONDITIONFILE')
            ctrl%InitCondFil = TRIM(remain)
            CALL echo('WAVE TRANSFORMATION FILE',ctrl%initcondfil)
            
        CASE ('CERCCOEFFICIENT')
            READ(remain,*,IOSTAT=ierr) ctrl%ls_param%K1
            IF (ierr/=0) CALL error_on_line('Error reading CERC coefficient on line')
            CALL echo('CERC COEFFICIENT', num2str(ctrl%ls_param%K1))
            
        CASE ('BC')
            Nbc = Nbc + 1
            ctrl%bc(Nbc)%typnam  = strtrim(remain(1:INDEX(remain,',',.FALSE.)-1))
            ctrl%bc(Nbc)%typnam = upper(ctrl%bc(Nbc)%typnam)
            ctrl%bc(Nbc)%typnam = strrep(ctrl%bc(Nbc)%typnam,' ','')
            remain = remain(INDEX(remain,',',.FALSE.)+1:)
            CALL process_bc_line(ctrl%bc(NBC)%typnam,remain)
            
        CASE ('OUTPUTDIR')
            ctrl%outdir = strtrim(remain)
            IF (ierr/=0) CALL error_on_line('Error reading output directory on line')
            CALL echo('OUTPUT DIR', strtrim(ctrl%outdir))
            
        CASE ('OUTPUT')
            Nout = Nout + 1
            tmpstr = strtrim(remain)
            tmpstr = upper(tmpstr)
            tmpstr = strrep(tmpstr,' ','')
            IF (ierr/=0) CALL error_on_line('Error reading output on line')
            CALL process_output_line(ctrl%out(Nout),tmpstr)
        
        CASE DEFAULT
            CALL error_on_line('Unrecognised flag encountered on line')
        END SELECT
    
    ! ENTERING BOUNDARY CONDITION SPECS
    ELSEIF (in_bc) THEN
        CALL process_bc_specs(ctrl%bc(Nbc),token,remain,in_bc)
        
    ! ENTERING BOUNDARY CONDITION SPECS
    ELSEIF (in_output) THEN
        CALL process_output_specs(ctrl%out(Nout),token,remain,in_output)
        
    END IF
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    IF (Nlerr==Nlerrmax) EXIT
END DO

CALL message('Finished echoing input file.','(a)')

IF (Nlerr/=0) THEN
    CALL err(sub,'Error reading input file'); RETURN
END IF

CALL message('Successful.','(a/)')

CONTAINS ! NESTED SUBROUTINES
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE error_on_line(str)
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
CHARACTER(LEN=*),INTENT(IN) :: str
! LOCAL VARIABLES
CHARACTER(LEN=MSTR) :: errstr

CALL message('','(a)')
CALL message('**********************************************************************','(a)')
CALL message(TRIM(str)//' '//num2str(Nline(Nfid)),'(a)')
CALL message(TRIM(line),'(a)')
CALL message('**********************************************************************','(a)')
CALL message('','(a)')
linestat = .FALSE.
Nlerr = Nlerr + 1

END SUBROUTINE error_on_line
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE echo(flag,str)
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
CHARACTER(LEN=*),INTENT(IN) :: flag
CHARACTER(LEN=*),INTENT(IN) :: str

IF (linestat) THEN
    CALL message(TRIM(flag)//' == '//TRIM(str),'(a)')
END IF

END SUBROUTINE echo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
FUNCTION next(str)
IMPLICIT NONE
! FUNCTION ARGUMENTS
CHARACTER(LEN=*),INTENT(IN) :: str
! FUNCTION RESULT
CHARACTER(LEN=LEN(str)) :: next
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: j

j = INDEX(str,',')
IF (j==0) THEN
    next = ''
ELSE
    next = str(j+1:)
END IF

END FUNCTION next
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE process_bc_line(typ,remain)
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
CHARACTER(LEN=*) :: typ
CHARACTER(LEN=*) :: remain
! LOCAL VARIABLES

SELECT CASE (typ)
CASE('WAVE','WATERLEVEL','TRANSPORT','SOURCE/SINK')
    ctrl%bc(Nbc)%fil = strtrim(remain)
    IF (ierr/=0) CALL error_on_line('Expecting timeseries filename on bc line')
    CALL echo('BC', TRIM(typ)//', '//ctrl%bc(Nbc)%fil)
CASE DEFAULT
    CALL error_on_line('Unrecognised boundary condition type on line')
END SELECT
in_bc = .TRUE.

END SUBROUTINE process_bc_line
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE process_output_line(out,typ)
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(OutCtrlTyp) :: out
CHARACTER(LEN=*) :: typ
! LOCAL VARIABLES

SELECT CASE (typ)
CASE('OFFSHOREWAVES')
    out%typnam = 'OFFSHOREWAVES'
    out%fil = fullfile(ctrl%outdir,TRIM(ctrl%evc)//'_wave_off','.csv')
CASE('INTERMEDIATEWAVES')
    out%typnam = 'INTERMEDIATEWAVES'
    out%fil = fullfile(ctrl%outdir,TRIM(ctrl%evc)//'_wave_int','.csv')
CASE('BREAKINGWAVES')
    out%typnam = 'BREAKINGWAVES'
    out%fil = fullfile(ctrl%outdir,TRIM(ctrl%evc)//'_wave_brk','.csv')
CASE('SHORELINE')
    out%typnam = 'SHORELINE'
    out%fil = fullfile(ctrl%outdir,TRIM(ctrl%evc)//'_shoreline','.csv')
CASE('LONGSHORE')
    out%typnam = 'LONGSHORE'
    out%fil = fullfile(ctrl%outdir,TRIM(ctrl%evc)//'_longshore','.csv')
CASE('TRANSPORT')
    out%typnam = 'TRANSPORT'
    out%fil = fullfile(ctrl%outdir,TRIM(ctrl%evc)//'_transport','.csv')
CASE('XSECT')
    out%typnam = 'XSECT'
    out%fil = fullfile(ctrl%outdir,TRIM(ctrl%evc)//'_xsect','.csv')
CASE('EQUILIBRIUMXSECT')
    out%typnam = 'EQUILIBRIUMXSECT'
    out%fil = fullfile(ctrl%outdir,TRIM(ctrl%evc)//'_eqxsect','.csv')
CASE('XSHORE')
    out%typnam = 'XSHORE'
    out%fil = fullfile(ctrl%outdir,TRIM(ctrl%evc)//'_xshore','.csv')
CASE('VOLUME')
    out%typnam = 'VOLUME'
    out%fil = fullfile(ctrl%outdir,TRIM(ctrl%evc)//'_vol','.csv')
CASE DEFAULT
    CALL error_on_line('Unrecognised output type on line')
END SELECT
CALL echo('OUTPUT', strtrim(typ))
in_output = .TRUE.

END SUBROUTINE process_output_line
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE process_bc_specs(bc,token,remain,in_bc)
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(BcCtrlTyp) :: bc
CHARACTER(LEN=*) :: token
CHARACTER(LEN=*) :: remain
LOGICAL :: in_bc
INTEGER(KIND=ISGL) :: i, itmp

SELECT CASE (token)
CASE('CHAINAGE')
    ALLOCATE(bc%chain(1), STAT=alloc_stat)
    IF (alloc_stat/=0) CALL error_on_line('Error reading bc chainage on line')
    READ(remain,*,IOSTAT=ierr) bc%chain(1)
    IF (ierr/=0) CALL error_on_line('Error reading bc chainage on line')
    CALL echo('  CHAINAGE',num2str(bc%chain(1)))
    
CASE('CHAINAGESTART/END')
    ALLOCATE(bc%chain(2), STAT=alloc_stat)
    IF (alloc_stat/=0) CALL error_on_line('Error reading bc chainage start/end on line')
    READ(remain,*,IOSTAT=ierr) bc%chain(1:2)
    IF (ierr/=0) CALL error_on_line('Error reading bc chainage on line')
    CALL echo('  CHAINAGE',num2str(bc%chain(1))//','//num2str(bc%chain(2)))

CASE ('LOOPBC')
    READ(remain,*,IOSTAT=ierr) bc%loop
    IF (ierr/=0) THEN
        CALL error_on_line('Error reading bc loop flag on line')
        RETURN
    ELSE IF (bc%loop) THEN
        CALL echo('  LOOP BC','.TRUE.')
    ELSE
        CALL echo('  LOOP BC','.FALSE.')
    END IF

CASE ('ENDBC')
    in_bc = .FALSE.
    CALL message('  END BC','(a)')
    
CASE ('BC')
    in_bc = .FALSE.
    CALL error_on_line('Expecting End BC flag on line')
    
CASE DEFAULT
    CALL error_on_line('Unrecognised BC flag encountered on line')

END SELECT

END SUBROUTINE process_bc_specs
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE process_output_specs(out,token,remain,in_output)
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(OutCtrlTyp) :: out
CHARACTER(LEN=*) :: token
CHARACTER(LEN=*) :: remain
LOGICAL :: in_output

SELECT CASE (token)

CASE ('STARTOUTPUT')
    READ(remain, '(a)', IOSTAT=ierr) timestr
    IF (ierr/=0) CALL error_on_line('Error reading start output time on line')
    out%tstart = timein(timestr,ctrl%tform,ctrl%tzero)
    CALL echo('  START OUTPUT', time2str(out%tstart,ctrl%tform,ctrl%tzero))
    
CASE ('FINALOUTPUT')
    READ(remain, '(a)', IOSTAT=ierr) timestr
    IF (ierr/=0) CALL error_on_line('Error reading final output time on line')
    out%tfinal = timein(timestr,ctrl%tform,ctrl%tzero)
    CALL echo('  FINAL OUTPUT', time2str(out%tfinal,ctrl%tform,ctrl%tzero))
    
CASE ('OUTPUTINTERVAL')
    READ(remain, *, IOSTAT=ierr) out%dt
    out%dt = out%dt * 86400. ! Convert from days to seconds
    IF (ierr/=0) CALL error_on_line('Error reading output interval on line')
    CALL echo('  OUTPUT INTERVAL', num2str(out%dt))

CASE ('OUTPUTLIST')
    out%info = strtrim(remain)
    IF (ierr/=0) CALL error_on_line('Error reading output list on line')
    CALL echo('  OUTPUT LIST', out%info)

CASE ('ENDOUTPUT')
    in_output = .FALSE.
    CALL message('END OUTPUT','(a)')
    
CASE ('OUTPUT')
    in_output = .FALSE.
    CALL error_on_line('Expecting End Output flag on line')
    
CASE DEFAULT
    CALL error_on_line('Unrecognised output flag encountered on line')
    
END SELECT

END SUBROUTINE process_output_specs
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE ctrl_construct
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
SUBROUTINE ctrl_destruct(ctrl)
! DESTRUCT SUBROUTINE

! SUBROUTINE DECLARATIONS
IMPLICIT NONE
! SUBROUTINE ARGUMENTS
TYPE(ctrl_typ),INTENT(INOUT) :: ctrl
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'ctrl_destruct'
INTEGER(KIND=ISGL) :: i
INTEGER :: tmpstat, dealloc_stat

DEALLOCATE(ctrl%bc, STAT=tmpstat)

END SUBROUTINE ctrl_destruct
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

END MODULE EVOCTRL
