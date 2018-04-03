! FVUTILGEN - FVWBM GENERAL UTILITY MODULE
!
! Copyright WBM 2007
! Authors: Ian Teakle

MODULE GEN_UTIL

INCLUDE 'COMPILER_DIRECTIVES.FI'

! MODULE USE STATEMENTS
USE PRECISION
USE GEN_ERROR

IMPLICIT NONE

! MODULE ACCESS STATEMENTS
PRIVATE
PUBLIC :: interp1
PUBLIC :: fzero, jtime, caldat, timein, time2str
PUBLIC :: kh_calc


! MODULE INTERFACE STATEMENTS
INTERFACE interp1
    MODULE PROCEDURE interp1_rs
!DEC$ IF (_PRECISION==1)    
    MODULE PROCEDURE interp1_ms, interp1_ds
!DEC$ END IF
END INTERFACE
!INTERFACE zbrac
!    MODULE PROCEDURE zbrac_sgl
!!DEC$ IF (_PRECISION==1)
!    MODULE PROCEDURE zbrac_dbl
!!DEC$ END IF
!END INTERFACE
INTERFACE fzero
    MODULE PROCEDURE fzero_sgl
!DEC$ IF (_PRECISION==1)
    MODULE PROCEDURE fzero_dbl
!DEC$ END IF
END INTERFACE
INTERFACE timein
    MODULE PROCEDURE convert_time_in
END INTERFACE
INTERFACE time2str
    MODULE PROCEDURE convert_time_out
END INTERFACE
INTERFACE kh_calc
    MODULE PROCEDURE kh_calc_r
!DEC$ IF (_PRECISION==1)
    MODULE PROCEDURE kh_calc_d
!DEC$ END IF
END INTERFACE

! MODULE PROCEDURES
CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
FUNCTION interp1_rs(x,y,xi) RESULT(yi)
IMPLICIT NONE
! FUNCTION ARGUMENTS
REAL(KIND=SGL),DIMENSION(:),INTENT(IN) :: x
REAL(KIND=SGL),DIMENSION(:),INTENT(IN) :: y
REAL(KIND=SGL),INTENT(IN) :: xi
! FUNCTION RESULT
REAL(KIND=SGL) :: yi
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: NN
INTEGER(KIND=ISGL) :: k

NN = SIZE(x)
k = maxloc(x-xi,MASK=x-xi .LE. 0,DIM=1)

IF (k==0) THEN
    yi = y(1)
ELSEIF (k==NN) THEN
    yi = y(NN)
ELSE
    yi = y(k) + (xi-x(k)) / (x(k+1)-x(k)) * (y(k+1)-y(k))
END IF

END FUNCTION interp1_rs
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DEC$ IF (_PRECISION==1)
FUNCTION interp1_ms(x,y,xi) RESULT(yi)
IMPLICIT NONE
! FUNCTION ARGUMENTS
REAL(KIND=DBL),DIMENSION(:),INTENT(IN) :: x
REAL(KIND=SGL),DIMENSION(:),INTENT(IN) :: y
REAL(KIND=DBL),INTENT(IN) :: xi
! FUNCTION RESULT
REAL(KIND=DBL) :: yi
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: NN
INTEGER(KIND=ISGL) :: k

NN = SIZE(x)

k = maxloc(x-xi,MASK=x-xi .LE. 0,DIM=1)

IF (k==0) THEN
    yi = y(1)
ELSEIF (k==NN) THEN
    yi = y(NN)
ELSE
    yi = y(k) + (xi-x(k)) / (x(k+1)-x(k)) * (y(k+1)-y(k))
END IF

END FUNCTION interp1_ms
!DEC$ END IF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DEC$ IF (_PRECISION==1)
FUNCTION interp1_ds(x,y,xi) RESULT(yi)
IMPLICIT NONE
! FUNCTION ARGUMENTS
REAL(KIND=DBL),DIMENSION(:),INTENT(IN) :: x
REAL(KIND=DBL),DIMENSION(:),INTENT(IN) :: y
REAL(KIND=DBL),INTENT(IN) :: xi
! FUNCTION RESULT
REAL(KIND=DBL) :: yi
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: NN
INTEGER(KIND=ISGL) :: k

NN = SIZE(x)

k = maxloc(x-xi,MASK=x-xi .LE. 0,DIM=1)

IF (k==0) THEN
    yi = y(1)
ELSEIF (k==NN) THEN
    yi = y(NN)
ELSE
    yi = y(k) + (xi-x(k)) / (x(k+1)-x(k)) * (y(k+1)-y(k))
END IF

END FUNCTION interp1_ds
!DEC$ END IF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DEC$ IF (_PRECISION==1)
SUBROUTINE fzero_dbl (func, B, C, R, RE, AE, IFLAG)
!***BEGIN PROLOGUE  FZERO
!***PURPOSE  Search for a zero of a function func(X) in a given interval
!            (B,C).  It is designed primarily for problems where func(B)
!            and func(C) have opposite signs.
!***LIBRARY   SLATEC
!***CATEGORY  F1B
!***TYPE      SINGLE PRECISION (FZERO-S, DFZERO-D)
!***KEYWORDS  BISECTION, NONLINEAR EQUATIONS, ROOTS, ZEROS
!***AUTHOR  Shampine, L. F., (SNLA)
!           Watts, H. A., (SNLA)
!***DESCRIPTION
!
!     FZERO searches for a zero of a REAL function func(X) between the
!     given REAL values B and C until the width of the interval (B,C)
!     has collapsed to within a tolerance specified by the stopping
!     criterion,
!        ABS(B-C) .LE. 2.*(RW*ABS(B)+AE).
!     The method used is an efficient combination of bisection and the
!     secant rule and is due to T. J. Dekker.
!
!     Description Of Arguments
!
!   F     :EXT   - Name of the REAL external function.  This name must
!                  be in an EXTERNAL statement in the calling program.
!                  F must be a function of one REAL argument.
!
!   B     :INOUT - One end of the REAL interval (B,C).  The value
!                  returned for B usually is the better approximation
!                  to a zero of F.
!
!   C     :INOUT - The other end of the REAL interval (B,C)
!
!   R     :OUT   - A (better) REAL guess of a zero of F which could help
!                  in speeding up convergence.  If func(B) and func(R) have
!                  opposite signs, a root will be found in the interval
!                  (B,R); if not, but func(R) and func(C) have opposite signs,
!                  a root will be found in the interval (R,C);
!                  otherwise, the interval (B,C) will be searched for a
!                  possible root.  When no better guess is known, it is
!                  recommended that r be set to B or C, since if R is
!                  not interior to the interval (B,C), it will be
!                  ignored.
!

!
!   IFLAG :OUT   - A status code.  User must check IFLAG after each
!                  call.  Control returns to the user from FZERO in all
!                  cases.
!
!                1  B is within the requested tolerance of a zero.
!                   The interval (B,C) collapsed to the requested
!                   tolerance, the function changes sign in (B,C), and
!                   func(X) decreased in magnitude as (B,C) collapsed.
!
!                2  func(B) = 0.  However, the interval (B,C) may not have
!                   collapsed to the requested tolerance.
!
!                3  B may be near a singular point of func(X).
!                   The interval (B,C) collapsed to the requested tol-
!                   erance and the function changes sign in (B,C), but
!                   func(X) increased in magnitude as (B,C) collapsed, i.e.
!                     ABS(func(B out)) .GT. MAX(ABS(func(B in)),ABS(func(C in)))
!
!                4  No change in sign of func(X) was found although the
!                   interval (B,C) collapsed to the requested tolerance.
!                   The user must examine this case and decide whether
!                   B is near a local minimum of func(X), or B is near a
!                   zero of even multiplicity, or neither of these.
!
!                5  Too many (.GT. 500) function evaluations used.
!
!***REFERENCES  L. F. Shampine and H. A. Watts, FZERO, a root-solving
!                 code, Report SC-TM-70-631, Sandia Laboratories,
!                 September 1970.
!               T. J. Dekker, Finding a zero by means of successive
!                 linear interpolation, Constructive Aspects of the
!                 Fundamental Theorem of Algebra, edited by B. Dejon
!                 and P. Henrici, Wiley-Interscience, 1969.
!***ROUTINES CALLED  R1MACH
!***REVISION HISTORY  (YYMMDD)
!   700901  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890531  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   920501  Reformatted the REFERENCES section.  (WRB)
!***END PROLOGUE  FZERO
IMPLICIT NONE
REAL(KIND=DBL), INTENT(INOUT) :: B,C,AE
INTEGER(KIND=ISGL), INTENT(OUT) :: IFLAG

REAL(KIND=DBL):: A,ACBS,ACMB,AW,CMB,ER,FA,FB,FC,FX,FZ,P,Q,R,RE,RW,T,TOL,Z
REAL(KIND=DBL), PARAMETER :: EPS=epsilon(B)
INTERFACE
    FUNCTION func(T)
    USE PRECISION
    IMPLICIT NONE
    REAL(KIND=DBL), INTENT(IN) :: T
    REAL(KIND=DBL) :: func
    END FUNCTION func
END INTERFACE
INTEGER IC,KOUNT

!***FIRST EXECUTABLE STATEMENT  FZERO
!
!   Initialize.
!
      Z = R
      IF (R .LE. MIN(B,C)  .OR.  R .GE. MAX(B,C)) Z = C
      RW = MAX(2.0_DBL*RE,EPS)
      AW = MAX(0.5_DBL*AE,0.E0)
      IC = 0
      T = Z
      FZ = func(T)
      FC = FZ
      T = B
      FB = func(T)
      KOUNT = 2
      IF (SIGN(1.0E0,FZ) .EQ. SIGN(1.0E0,FB)) GO TO 1
      C = Z
      GO TO 2
    1 IF (Z .EQ. C) GO TO 2
      T = C
      FC = func(T)
      KOUNT = 3
      IF (SIGN(1.0E0,FZ) .EQ. SIGN(1.0E0,FC)) GO TO 2
      B = Z
      FB = FZ
    2 A = C
      FA = FC
      ACBS = ABS(B-C)
      FX = MAX(ABS(FB),ABS(FC))
!
    3 IF (ABS(FC) .GE. ABS(FB)) GO TO 4
!
!   Perform interchange.
!
      A = B
      FA = FB
      B = C
      FB = FC
      C = A
      FC = FA
!
    4 CMB = 0.5E0*(C-B)
      ACMB = ABS(CMB)
      TOL = RW*ABS(B) + AW
!
!   Test stopping criterion and function count.
!
      IF (ACMB .LE. TOL) GO TO 10
      IF (FB .EQ. 0.E0) GO TO 11
      IF (KOUNT .GE. 500) GO TO 14
!
!   Calculate new iterate implicitly as B+P/Q, where we arrange
!   P .GE. 0.  The implicit form is used to prevent overflow.
!
      P = (B-A)*FB
      Q = FA - FB
      IF (P .GE. 0.E0) GO TO 5
      P = -P
      Q = -Q
!
!   Update A and check for satisfactory reduction in the size of the
!   bracketing interval.  If not, perform bisection.
!
    5 A = B
      FA = FB
      IC = IC + 1
      IF (IC .LT. 4) GO TO 6
      IF (8.0E0*ACMB .GE. ACBS) GO TO 8
      IC = 0
      ACBS = ACMB
!
!   Test for too small a change.
!
    6 IF (P .GT. ABS(Q)*TOL) GO TO 7
!
!   Increment by TOLerance.
!
      B = B + SIGN(TOL,CMB)
      GO TO 9
!
!   Root ought to be between B and (C+B)/2.
!
    7 IF (P .GE. CMB*Q) GO TO 8
!
!   Use secant rule.
!
      B = B + P/Q
      GO TO 9
!
!   Use bisection (C+B)/2.
!
    8 B = B + CMB
!
!   Have completed computation for new iterate B.
!
    9 T = B
      FB = func(T)
      KOUNT = KOUNT + 1
!
!   Decide whether next step is interpolation or extrapolation.
!
      IF (SIGN(1.0E0,FB) .NE. SIGN(1.0E0,FC)) GO TO 3
      C = A
      FC = FA
      GO TO 3
!
!   Finished.  Process results for proper setting of IFLAG.
!
   10 IF (SIGN(1.0E0,FB) .EQ. SIGN(1.0E0,FC)) GO TO 13
      IF (ABS(FB) .GT. FX) GO TO 12
      IFLAG = 1
      RETURN
   11 IFLAG = 2
      RETURN
   12 IFLAG = 3
      RETURN
   13 IFLAG = 4
      RETURN
   14 IFLAG = 5
      RETURN
END SUBROUTINE fzero_dbl
!DEC$ END IF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE fzero_sgl (func, B, C, R, RE, AE, IFLAG)
!***BEGIN PROLOGUE  FZERO
!***PURPOSE  Search for a zero of a function func(X) in a given interval
!            (B,C).  It is designed primarily for problems where func(B)
!            and func(C) have opposite signs.
!***LIBRARY   SLATEC
!***CATEGORY  F1B
!***TYPE      SINGLE PRECISION (FZERO-S, DFZERO-D)
!***KEYWORDS  BISECTION, NONLINEAR EQUATIONS, ROOTS, ZEROS
!***AUTHOR  Shampine, L. F., (SNLA)
!           Watts, H. A., (SNLA)
!***DESCRIPTION
!
!     FZERO searches for a zero of a REAL function func(X) between the
!     given REAL values B and C until the width of the interval (B,C)
!     has collapsed to within a tolerance specified by the stopping
!     criterion,
!        ABS(B-C) .LE. 2.*(RW*ABS(B)+AE).
!     The method used is an efficient combination of bisection and the
!     secant rule and is due to T. J. Dekker.
!
!     Description Of Arguments
!
!   F     :EXT   - Name of the REAL external function.  This name must
!                  be in an EXTERNAL statement in the calling program.
!                  F must be a function of one REAL argument.
!
!   B     :INOUT - One end of the REAL interval (B,C).  The value
!                  returned for B usually is the better approximation
!                  to a zero of F.
!
!   C     :INOUT - The other end of the REAL interval (B,C)
!
!   R     :OUT   - A (better) REAL guess of a zero of F which could help
!                  in speeding up convergence.  If func(B) and func(R) have
!                  opposite signs, a root will be found in the interval
!                  (B,R); if not, but func(R) and func(C) have opposite signs,
!                  a root will be found in the interval (R,C);
!                  otherwise, the interval (B,C) will be searched for a
!                  possible root.  When no better guess is known, it is
!                  recommended that r be set to B or C, since if R is
!                  not interior to the interval (B,C), it will be
!                  ignored.
!

!
!   IFLAG :OUT   - A status code.  User must check IFLAG after each
!                  call.  Control returns to the user from FZERO in all
!                  cases.
!
!                1  B is within the requested tolerance of a zero.
!                   The interval (B,C) collapsed to the requested
!                   tolerance, the function changes sign in (B,C), and
!                   func(X) decreased in magnitude as (B,C) collapsed.
!
!                2  func(B) = 0.  However, the interval (B,C) may not have
!                   collapsed to the requested tolerance.
!
!                3  B may be near a singular point of func(X).
!                   The interval (B,C) collapsed to the requested tol-
!                   erance and the function changes sign in (B,C), but
!                   func(X) increased in magnitude as (B,C) collapsed, i.e.
!                     ABS(func(B out)) .GT. MAX(ABS(func(B in)),ABS(func(C in)))
!
!                4  No change in sign of func(X) was found although the
!                   interval (B,C) collapsed to the requested tolerance.
!                   The user must examine this case and decide whether
!                   B is near a local minimum of func(X), or B is near a
!                   zero of even multiplicity, or neither of these.
!
!                5  Too many (.GT. 500) function evaluations used.
!
!***REFERENCES  L. F. Shampine and H. A. Watts, FZERO, a root-solving
!                 code, Report SC-TM-70-631, Sandia Laboratories,
!                 September 1970.
!               T. J. Dekker, Finding a zero by means of successive
!                 linear interpolation, Constructive Aspects of the
!                 Fundamental Theorem of Algebra, edited by B. Dejon
!                 and P. Henrici, Wiley-Interscience, 1969.
!***ROUTINES CALLED  R1MACH
!***REVISION HISTORY  (YYMMDD)
!   700901  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890531  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   920501  Reformatted the REFERENCES section.  (WRB)
!***END PROLOGUE  FZERO
IMPLICIT NONE
REAL(KIND=SGL), INTENT(INOUT) :: B,C,AE
INTEGER(KIND=ISGL), INTENT(OUT) :: IFLAG

REAL(KIND=SGL):: A,ACBS,ACMB,AW,CMB,ER,FA,FB,FC,FX,FZ,P,Q,R,RE,RW,T,TOL,Z
REAL(KIND=SGL), PARAMETER :: EPS=epsilon(B)
INTERFACE
    FUNCTION func(T)
    USE PRECISION
    IMPLICIT NONE
    REAL(KIND=SGL), INTENT(IN) :: T
    REAL(KIND=SGL) :: func
    END FUNCTION func
END INTERFACE
INTEGER IC,KOUNT

!***FIRST EXECUTABLE STATEMENT  FZERO
!
!   Initialize.
!
      Z = R
      IF (R .LE. MIN(B,C)  .OR.  R .GE. MAX(B,C)) Z = C
      RW = MAX(2.0_DBL*RE,EPS)
      AW = MAX(0.5_DBL*AE,0.E0)
      IC = 0
      T = Z
      FZ = func(T)
      FC = FZ
      T = B
      FB = func(T)
      KOUNT = 2
      IF (SIGN(1.0E0,FZ) .EQ. SIGN(1.0E0,FB)) GO TO 1
      C = Z
      GO TO 2
    1 IF (Z .EQ. C) GO TO 2
      T = C
      FC = func(T)
      KOUNT = 3
      IF (SIGN(1.0E0,FZ) .EQ. SIGN(1.0E0,FC)) GO TO 2
      B = Z
      FB = FZ
    2 A = C
      FA = FC
      ACBS = ABS(B-C)
      FX = MAX(ABS(FB),ABS(FC))
!
    3 IF (ABS(FC) .GE. ABS(FB)) GO TO 4
!
!   Perform interchange.
!
      A = B
      FA = FB
      B = C
      FB = FC
      C = A
      FC = FA
!
    4 CMB = 0.5E0*(C-B)
      ACMB = ABS(CMB)
      TOL = RW*ABS(B) + AW
!
!   Test stopping criterion and function count.
!
      IF (ACMB .LE. TOL) GO TO 10
      IF (FB .EQ. 0.E0) GO TO 11
      IF (KOUNT .GE. 500) GO TO 14
!
!   Calculate new iterate implicitly as B+P/Q, where we arrange
!   P .GE. 0.  The implicit form is used to prevent overflow.
!
      P = (B-A)*FB
      Q = FA - FB
      IF (P .GE. 0.E0) GO TO 5
      P = -P
      Q = -Q
!
!   Update A and check for satisfactory reduction in the size of the
!   bracketing interval.  If not, perform bisection.
!
    5 A = B
      FA = FB
      IC = IC + 1
      IF (IC .LT. 4) GO TO 6
      IF (8.0E0*ACMB .GE. ACBS) GO TO 8
      IC = 0
      ACBS = ACMB
!
!   Test for too small a change.
!
    6 IF (P .GT. ABS(Q)*TOL) GO TO 7
!
!   Increment by TOLerance.
!
      B = B + SIGN(TOL,CMB)
      GO TO 9
!
!   Root ought to be between B and (C+B)/2.
!
    7 IF (P .GE. CMB*Q) GO TO 8
!
!   Use secant rule.
!
      B = B + P/Q
      GO TO 9
!
!   Use bisection (C+B)/2.
!
    8 B = B + CMB
!
!   Have completed computation for new iterate B.
!
    9 T = B
      FB = func(T)
      KOUNT = KOUNT + 1
!
!   Decide whether next step is interpolation or extrapolation.
!
      IF (SIGN(1.0E0,FB) .NE. SIGN(1.0E0,FC)) GO TO 3
      C = A
      FC = FA
      GO TO 3
!
!   Finished.  Process results for proper setting of IFLAG.
!
   10 IF (SIGN(1.0E0,FB) .EQ. SIGN(1.0E0,FC)) GO TO 13
      IF (ABS(FB) .GT. FX) GO TO 12
      IFLAG = 1
      RETURN
   11 IFLAG = 2
      RETURN
   12 IFLAG = 3
      RETURN
   13 IFLAG = 4
      RETURN
   14 IFLAG = 5
      RETURN
END SUBROUTINE fzero_sgl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ELEMENTAL FUNCTION JTime(yy,mm,dd,thh,tmm,tss) RESULT(time) 
! RETURN A JULIAN TIME (DECIMAL DAYS) GIVEN INTEGER INPUT (YYYY,MM,DD,THH,TMM,TSS)
! COURTESY OF DAVE CALLAGHAN, ORIGINALLY BASED ON NUMERICAL RECIPES
IMPLICIT NONE
! FUNCTION ARGUMENTS
INTEGER(KIND=ISGL), INTENT(IN) :: yy,mm,dd,thh,tmm,tss
! FUNCTION RESULT
REAL(KIND=DBL) :: time
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: JulNo
INTEGER(KIND=ISGL),PARAMETER :: JulConst=15+31*(10+12*1582)
INTEGER(KIND=ISGL) :: ja,jm,jy
jy=yy
IF (jy == 0) THEN
	time=-1.0d0
ELSE
	IF (jy < 0) jy=jy+1
	IF (mm > 2) THEN
		jm=mm+1
	ELSE
		jy=jy-1
		jm=mm+13
	END IF
	JulNo=FLOOR(365.25d0*jy)+FLOOR(30.6001d0*jm)+dd+1720995
	IF (dd+31*(mm+12*yy) >= JulConst) THEN
		ja=FLOOR(0.01d0*jy)
		JulNo=JulNo+2-ja+FLOOR(0.25d0*ja)
	END IF
	time=1.0d0*JulNo+((tss/60.0d0+tmm)/60.0d0+thh)/24.0d0
END IF
END FUNCTION JTime
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ELEMENTAL FUNCTION ISODateToJTime(ISODate) RESULT(JulianTime)
! RETURN A DECIMAL DAY JULIAN TIME GIVEN ISO DATE/TIME STRING 'DD/MM/YYYY HH:MM:SS'
IMPLICIT NONE
! FUNCTION ARGUMENT
CHARACTER(LEN=*),INTENT(in) :: ISODate
! FUNCTION RESULT
REAL(KIND=DBL) :: JulianTime
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: len
INTEGER(KIND=ISGL) :: year,month,day,hour,min,sec
INTEGER(KIND=ISGL) :: IORes

len = LEN_TRIM(ISODate)
IORes = -9999
IF (len==10) THEN
    READ(ISODate,'(I2,X,I2,X,I4)',IOSTAT=IORes) day,month,year
    hour = 0; min = 0; sec = 0
ELSEIF (len==13) THEN
    READ(ISODate,'(I2,X,I2,X,I4,X,I2)',IOSTAT=IORes) day,month,year,hour
    min = 0; sec = 0
ELSEIF (len==16) THEN
    READ(ISODate,'(I2,X,I2,X,I4,X,I2,X,I2)',IOSTAT=IORes) day,month,year,hour,min
    sec = 0
ELSEIF (len==19) THEN
    READ(ISODate,'(I2,X,I2,X,I4,X,I2,X,I2,X,I2)',IOSTAT=IORes) day,month,year,hour,min,sec
END IF
IF (IORes==0) THEN
    JulianTime = JTime(year,month,day,hour,min,sec)
ELSE
    JulianTime = NAN_DBL
END IF

END FUNCTION ISODateToJTime
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ELEMENTAL FUNCTION JTimeToISODate(JTime) RESULT(ISODate)
! RETURN AN ISO DATE/TIME STRING 'DD/MM/YYYY HH:MM:SS' GIVEN A DECIMAL DAY JULIAN TIME
IMPLICIT NONE
! FUNCTION ARGUMENT
REAL(KIND=DBL),INTENT(IN) :: JTime
! FUNCTION RESULT
CHARACTER(LEN=19) :: ISODate
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: year,month,day,hour,min,sec
INTEGER(KIND=ISGL) :: TmpI,i
REAL(KIND=DBL) :: TmpR
CHARACTER(LEN=19)::TmpStr
INTEGER(KIND=ISGL) :: IORes

TmpI=FLOOR(JTime)
TmpR=(JTime-tmpI)*24.0d0
hour=FLOOR(TmpR)
min=FLOOR(TmpR*60.0d0-hour*60.0d0)
sec=NINT(TmpR*3600.0d0-hour*3600.0d0-min*60.0d0)
IF (sec==60) THEN
    sec=0
    min=min+1
    IF (min==60) THEN
        min=0
        hour=hour+1
        IF (hour==24) THEN
            hour=0
            tmpI=tmpI+1
        END IF
    END IF
END IF
CALL caldat(TmpI,month,day,year)
WRITE(TmpStr,'(I2.2"/"I2.2"/"I4.4" "I2.2":"I2.2":"I2.2)',IOSTAT=IORes)&
    day,month,year,hour,min,sec
IF (IORes==0) THEN
    ISODate=TmpStr
ELSE
    ISODate='NaN'
END IF

END FUNCTION JTimeToISODate
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PURE SUBROUTINE caldat(julian,mm,id,iyyy)
IMPLICIT NONE
INTEGER(KIND=ISGL), INTENT(IN) :: julian
INTEGER(KIND=ISGL), INTENT(OUT) :: mm,id,iyyy
INTEGER(KIND=ISGL) :: ja,jalpha,jb,jc,jd,je
INTEGER(KIND=ISGL), PARAMETER :: IGREG=2299161
IF (julian >= IGREG) THEN
	jalpha=INT(((julian-1867216)-0.25d0)/36524.25d0)
	ja=julian+1+jalpha-INT(0.25d0*jalpha)
ELSE IF (julian < 0) THEN
	ja=julian+36525*(1-julian/36525)
ELSE
	ja=julian
END IF
jb=ja+1524
jc=INT(6680.0d0+((jb-2439870)-122.1d0)/365.25d0)
jd=365*jc+INT(0.25d0*jc)
je=INT((jb-jd)/30.6001d0)
id=jb-jd-INT(30.6001d0*je)
mm=je-1
IF (mm > 12) mm=mm-12
iyyy=jc-4715
IF (mm > 2) iyyy=iyyy-1
IF (iyyy <= 0) iyyy=iyyy-1
IF (julian < 0) iyyy=iyyy-100*(1-julian/36525)
END SUBROUTINE caldat
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ELEMENTAL FUNCTION convert_time_in(tstring,tform,tzero) RESULT(t)
! RETURN MODEL TIME IN DECIMAL SECONDS GIVEN TIME CHARACTER INPUT AND FORMAT SPECIFICATION
IMPLICIT NONE
! FUNCTION ARGUMENTS
CHARACTER(LEN=*),INTENT(IN) :: tstring
INTEGER(KIND=ISGL),INTENT(IN) :: tform
REAL(KIND=DBL),OPTIONAL,INTENT(IN) :: tzero
! FUNCTION RESULT
REAL(KIND=DBL) :: t
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: IORes

IF (tform==0) THEN  ! DECIMAL HOURS TO SECONDS
    READ(tstring,*,IOSTAT=IORes) t
    IF (IORes==0) THEN
        t = t * 86400._DBL
    ELSE
        t = NAN_DBL
    END IF
ELSEIF (tform==1) THEN ! ISODATE TO SECONDS
    t = ISODateToJTime(tstring)
    t = t * 86400._DBL
    IF (PRESENT(tzero)) t = t - tzero
END IF

END FUNCTION convert_time_in
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ELEMENTAL FUNCTION convert_time_out(t,tform,tzero) RESULT(tstring)
! RETURN MODEL TIME AS A CHARACTER OUTPUT ACCORDING TO FORMAT SPECIFICATION
IMPLICIT NONE
! FUNCTION ARGUMENTS
REAL(KIND=DBL),INTENT(IN) :: t
INTEGER(KIND=ISGL),INTENT(IN) :: tform
REAL(KIND=DBL),OPTIONAL,INTENT(IN) :: tzero
! FUNCTION RESULT
CHARACTER(LEN=19) :: tstring
! LOCAL VARIABLES
REAL(KIND=DBL) :: tmp
INTEGER(KIND=ISGL) :: IORes

IF (tform==0) THEN  ! SECONDS TO DECIMAL DAY STRING
    WRITE(tstring,'(F15.2)',IOSTAT=IORes) t/86400._DBL
    IF (IORes/=0) tstring = 'NaN'
ELSEIF (tform==1) THEN ! SECONDS TO ISODATE STRING
    tmp = t
    IF (PRESENT(tzero)) tmp = tmp + tzero
    tstring = JTimeToISODate(tmp/86400._DBL)
END IF

END FUNCTION convert_time_out
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PURE FUNCTION kh_calc_r(k0h) RESULT(kh)
! CALCULATE WAVENUMBER GIVEN DEEPWATER VALUE

! FUNCTION DECLARATIONS
IMPLICIT NONE
! FUNCTION ARGUMENTS
REAL(KIND=SGL),INTENT(IN) :: k0h
! FUNCTION RESULT
REAL(KIND=SGL) :: kh
! LOCAL VARIABLES
INTEGER(KIND=ISGL),PARAMETER :: Nmax = 10
REAL(KIND=SGL),PARAMETER :: tol = 0.001_SGL
INTEGER(KIND=ISGL) :: n
REAL(KIND=SGL) :: tkh,f,df,khnew,err


IF (k0h<0.) THEN
    kh = 0.; RETURN
ELSE IF (k0h<2.72) THEN
    kh = SQRT(k0h) * (1.+1./6.*k0h+11./360.*k0h*k0h)
ELSE
    kh = k0h
END IF
n = 1
err = 9999.
DO
    IF (n>Nmax.OR.err<tol) EXIT
    tkh = TANH(kh)
    f = kh*tkh-k0h
    df = tkh+kh*(1.-tkh*tkh)
    khnew = kh-f/df
    err = ABS(khnew-kh)/kh
    kh = khnew
    n = n + 1
END DO

END FUNCTION kh_calc_r
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DEC$ IF (_PRECISION==1)
PURE FUNCTION kh_calc_d(k0h) RESULT(kh)
! CALCULATE WAVENUMBER GIVEN DEEPWATER VALUE

! FUNCTION DECLARATIONS
IMPLICIT NONE
! FUNCTION ARGUMENTS
REAL(KIND=DBL),INTENT(IN) :: k0h
! FUNCTION RESULT
REAL(KIND=DBL) :: kh
! LOCAL VARIABLES
INTEGER(KIND=ISGL),PARAMETER :: Nmax = 10
REAL(KIND=DBL),PARAMETER :: tol = 0.001_DBL
INTEGER(KIND=ISGL) :: n
REAL(KIND=DBL) :: tkh,f,df,khnew,err

IF (k0h<0._DBL) THEN
    kh = 0._DBL; RETURN
ELSE IF (k0h<2.72_DBL) THEN
    kh = SQRT(k0h) * (1._DBL+1._DBL/6._DBL*k0h+11._DBL/360._DBL*k0h*k0h)
ELSE
    kh = k0h
END IF
n = 1
err = 9999._DBL
DO
    IF (n>Nmax.OR.err<tol) EXIT
    tkh = TANH(kh)
    f = kh*tkh-k0h
    df = tkh+kh*(1.-tkh*tkh)
    khnew = kh-f/df
    err = ABS(khnew-kh)/kh
    kh = khnew
    n = n + 1
END DO

END FUNCTION kh_calc_d
!DEC$ ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE GEN_UTIL

MODULE FUNC_PARAMS
! MODULE USE STATMENTS
USE PRECISION
! MODULE DECLARATIONS
IMPLICIT NONE
! MODULE VARIABLE DECLARATIONS
REAL(KIND=SGL) :: a,b,c,d,e

END MODULE FUNC_PARAMS
