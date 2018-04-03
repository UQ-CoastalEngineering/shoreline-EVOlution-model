! GEN_STRING - Generic String Handling Procedures Module
!
! Copyright WBM 2007
! Authors: Ian Teakle

MODULE GEN_STRING
!!DEC$ STRICT

INCLUDE 'COMPILER_DIRECTIVES.FI'

! MODULE USE STATEMENTS
USE PRECISION

! MODULE ACCESS STATEMENTS
PRIVATE
PUBLIC :: strtrim, upper, lower, iscomment, removecomments, pad
PUBLIC :: strfind, strrep, parse, strmatch, num2str

! MODULE INTERFACE STATEMENTS
INTERFACE upper
    MODULE PROCEDURE upper_s, upper_v
END INTERFACE
INTERFACE lower
    MODULE PROCEDURE lower_s, lower_v
END INTERFACE
INTERFACE strfind
    MODULE PROCEDURE strfind_all, strfind_1only, strfind_n
END INTERFACE
INTERFACE strrep
    MODULE PROCEDURE strrep_all, strrep_1only
END INTERFACE
INTERFACE strmatch
    MODULE PROCEDURE strmatch_all, strmatch_1only
END INTERFACE
INTERFACE num2str
    MODULE PROCEDURE int2str_s, int2str_v, real2str_s, real2str_v
!DEC$ IF (_PRECISION==1)
    MODULE PROCEDURE dbl2str_s, dbl2str_v
!DEC$ END IF
END INTERFACE

! MODULE PROCEDURES
CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PURE FUNCTION strtrim(str1) RESULT(str2)
! TRIMS BLANKS FROM FRONT AND BACK
IMPLICIT NONE
! FUNCTION ARGUMENTS
CHARACTER(LEN=*),INTENT(IN) :: str1
! FUNCTION RESULT
CHARACTER(LEN=LEN_TRIM(ADJUSTL(str1))) :: str2

str2 = TRIM(ADJUSTL(str1))

END FUNCTION strtrim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PURE FUNCTION upper_s(str1) RESULT(str2)
! CONVERTS TO UPPER CASE
IMPLICIT NONE
! FUNCTION ARGUMENTS
CHARACTER(LEN=*),INTENT(IN) :: str1
! FUNCTION RESULT
CHARACTER(LEN=LEN(str1)) :: str2
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: i

str2 = str1
DO i = 1, LEN_TRIM(str2)
    IF (LGE(str2(i:i),'a') .AND. LLE(str2(i:i),'z')) THEN
        str2(i:i) = ACHAR(IACHAR(str2(i:i)) - 32)
    END IF
END DO

END FUNCTION
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PURE FUNCTION upper_v(str1) RESULT(str2)
! CONVERTS TO UPPER CASE
IMPLICIT NONE
! FUNCTION ARGUMENTS
CHARACTER(LEN=*),DIMENSION(:),INTENT(IN) :: str1
! FUNCTION RESULT
CHARACTER(LEN=LEN(str1)),DIMENSION(SIZE(str1)) :: str2
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: i, j

str2 = str1
DO i = 1,SIZE(str2)
    DO j = 1, LEN_TRIM(str2(i))
        IF (LGE(str2(i)(j:j),'a') .AND. LLE(str2(i)(j:j),'z')) THEN
            str2(i)(j:j) = ACHAR(IACHAR(str2(i)(j:j)) - 32)
        END IF
    END DO
END DO

END FUNCTION
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PURE FUNCTION lower_s(str1) RESULT(str2)
! CONVERTS TO LOWER CASE
IMPLICIT NONE
! FUNCTION ARGUMENTS
CHARACTER(LEN=*),INTENT(IN) :: str1
! FUNCTION RESULT
CHARACTER(LEN=LEN(str1)) :: str2
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: i

str2 = str1
DO i = 1, LEN_TRIM(str2)
    IF (LGE(str2(i:i),'A') .AND. LLE(str2(i:i),'Z')) THEN
        str2(i:i) = ACHAR(IACHAR(str2(i:i)) + 32)
    END IF
END DO

END FUNCTION
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PURE FUNCTION lower_v(str1) RESULT(str2)
! CONVERTS TO UPPER CASE
IMPLICIT NONE
! FUNCTION ARGUMENTS
CHARACTER(LEN=*),DIMENSION(:),INTENT(IN) :: str1
! FUNCTION RESULT
CHARACTER(LEN=LEN(str1)),DIMENSION(SIZE(str1)) :: str2
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: i, j

str2 = str1
DO i = 1,SIZE(str2)
    DO j = 1, LEN_TRIM(str2(i))
        IF (LGE(str2(i)(j:j),'a') .AND. LLE(str2(i)(j:j),'z')) THEN
            str2(i)(j:j) = ACHAR(IACHAR(str2(i)(j:j)) + 32)
        END IF
    END DO
END DO

END FUNCTION
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PURE FUNCTION iscomment(str) RESULT(stat)
! IS IT A COMMENT LINE (TRUE/FALSE)
IMPLICIT NONE
! FUNCTION ARGUMENTS
CHARACTER(LEN=*),INTENT(IN) :: str
! FUNCTION RESULT
LOGICAL :: stat
! LOCAL VARIABLES
CHARACTER(LEN=LEN(str)) :: tmpstr

tmpstr = trim(ADJUSTL(str))
stat = tmpstr(1:1)=="#" .OR. tmpstr(1:1)=="!" .OR. len_trim(tmpstr)==0

END FUNCTION iscomment
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PURE FUNCTION removecomments(str1) RESULT(str2)
! REMOVE TRAILING COMMENTS FROM A STRING
IMPLICIT NONE
! FUNCTION ARGUMENTS
CHARACTER(LEN=*),INTENT(IN) :: str1
! FUNCTION RESULT
CHARACTER(LEN=LEN(str1)) :: str2
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: i, ih, ie

str2 = ''
ih = INDEX(str1,"#")
ie = INDEX(str1,"!")
IF (ih/=0 .AND. ie/=0) THEN
    i = MIN(ih, ie)
ELSE
    i = MAX(ih, ie)
END IF
IF (i>0) THEN
    str2(:i-1) = str1(:i-1)
ELSE
    str2 = str1
END IF

END FUNCTION removecomments
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PURE FUNCTION pad(str1,len2) RESULT(str2)
! PAD LENGTH OF STR1 TO HAVE LEN=LEN2
IMPLICIT NONE
! FUNCTION ARGUMENTS
CHARACTER(LEN=*),INTENT(IN) :: str1
INTEGER(KIND=ISGL),INTENT(IN) :: len2
! FUNCTION RESULT
CHARACTER(LEN=len2) :: str2
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: len1

len1 = LEN(str1)
str2 = ''
IF (len2>=len1) THEN
    str2(1:len1) = str1
ELSE
    str2 = str1(1:len2)
END IF

END FUNCTION pad
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PURE FUNCTION strfind_all(str,pat) RESULT(k)
! FIND POSITION/S (WITHIN STR) OF ANY INSTANCES OF PAT
IMPLICIT NONE
! FUNCTION ARGUMENTS
CHARACTER(LEN=*),INTENT(IN) :: str
CHARACTER(LEN=*),INTENT(IN) :: pat
! FUNCTION RESULT
INTEGER(KIND=ISGL),ALLOCATABLE,DIMENSION(:) :: k
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: i, Nk, Ns, Np

Ns = LEN_TRIM(str)
Np = MAX(1,LEN_TRIM(pat))
IF ((Ns==0).OR.(Np>Ns)) THEN
    Nk = 1
    ALLOCATE(k(Nk))
    k = 0
    RETURN
END IF

Nk = 0
DO i = 1,(Ns-Np+1)
    IF (str(i:i+Np-1)==pat(1:Np)) Nk = Nk + 1
END DO

IF (Nk==0) THEN
    Nk = 1
    ALLOCATE(k(Nk))
    k = 0
    RETURN
ELSE
    ALLOCATE(k(Nk))
    k = 0
END IF

Nk = 0
DO i = 1,(Ns-Np+1)
    IF (str(i:i+Np-1)==pat) THEN
        Nk = Nk + 1
        k(Nk) = i
    END IF
END DO

END FUNCTION strfind_all
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PURE FUNCTION strfind_1only(str,pat,back) RESULT(k)
! FIND POSITION/S (WITHIN STR) OF ANY INSTANCES OF PAT
IMPLICIT NONE
! FUNCTION ARGUMENTS
CHARACTER(LEN=*),INTENT(IN) :: str
CHARACTER(LEN=*),INTENT(IN) :: pat
LOGICAL,INTENT(IN) :: back
! FUNCTION RESULT
INTEGER(KIND=ISGL) :: k
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: i, Ns, Np

k = 0
Ns = LEN_TRIM(str)
Np = MAX(1,LEN_TRIM(pat))
IF ((Ns==0).OR.(Np>Ns)) RETURN

IF (.NOT.back) THEN
    DO i = 1,(Ns-Np+1)
        IF (str(i:i+Np-1)==pat) THEN
            k = i
            EXIT
        END IF
    END DO
ELSE
    DO i = (Ns-Np+1),1,-1
        IF (str(i:i+Np-1)==pat) THEN
            k = i
            EXIT
        END IF
    END DO
END IF

END FUNCTION strfind_1only
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PURE FUNCTION strfind_n(str,pat,n,back) RESULT(k)
! FIND THE NTH INSTANCE OF PAT IN STR
! IF THERE ARE LESS THAN N INSTANCES THEN THE POSITION OF THE LAST ENCOUNTERED IS RETURNED
! IF THERE ARE NO INSTANCES THEN 0 IS RETURNED
! IF N IS ZERO AND BACK IS ABSENT OR FALSE THEN K=0.
! IF N IS ZEROS AND BACK IS TRUE THEN K=LEN_TRIM(STR)
IMPLICIT NONE
! FUNCTION ARGUMENTS
CHARACTER(LEN=*),INTENT(IN) :: str
CHARACTER(LEN=*),INTENT(IN) :: pat
INTEGER(KIND=ISGL),INTENT(IN) :: n
LOGICAL,OPTIONAL,INTENT(IN) :: back
! FUNCTION RESULT
INTEGER(KIND=ISGL) :: k
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: i, Nk, Ns, Np
LOGICAL :: bck

IF (.NOT.PRESENT(back)) THEN
    bck = .false.
ELSE
    bck = back
END IF

k = 0
Nk = 0
Ns = LEN_TRIM(str)
Np = MAX(1,LEN_TRIM(pat))
IF ((Ns==0).OR.(Np>Ns)) RETURN

IF (n==0) THEN
    IF (.NOT.bck) THEN
        k = 0
    ELSE
        k = Ns
    END IF
    RETURN
END IF

IF (.NOT.bck) THEN
    DO i = 1,(Ns-Np+1)
        IF (str(i:i+Np-1)==pat) THEN
            k = i
            Nk = Nk + 1
            IF (Nk==n) EXIT
        END IF
    END DO
ELSE
    DO i = (Ns-Np+1),1,-1
        IF (str(i:i+Np-1)==pat) THEN
            k = i
            Nk = Nk + 1
            IF (Nk==n) EXIT
        END IF
    END DO
END IF

END FUNCTION strfind_n
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PURE FUNCTION strrep_all(str1,pat1,pat2) RESULT(str2)
! REPLACE (WITHIN STR1) ANY INSTANCES OF PAT1 WITH PAT2
IMPLICIT NONE
! FUNCTION ARGUMENTS
CHARACTER(LEN=*),INTENT(IN) :: str1
CHARACTER(LEN=*),INTENT(IN) :: pat1
CHARACTER(LEN=*),INTENT(IN) :: pat2
! FUNCTION RESULT
CHARACTER(LEN=:),ALLOCATABLE :: str2
! LOCAL VARIABLES
INTEGER(KIND=ISGL),DIMENSION(LEN_TRIM(str1)) :: k
INTEGER(KIND=ISGL) :: Ns1 ! NON-BLANK LENGTH OF STR1
INTEGER(KIND=ISGL) :: Ns2 ! NON-BLANK LENGTH OF STR2
INTEGER(KIND=ISGL) :: Np1 ! NON-BLANK LENGTH OF PAT1 (>=1)
INTEGER(KIND=ISGL) :: Np2 ! NON-BLANK LENGTH OF PAT2
INTEGER(KIND=ISGL) :: Nk  ! NUMBER OF INSTANCES OF PAT1 IN STR1
INTEGER(KIND=ISGL) :: i1, i2, j1, j2, n

Ns1 = LEN_TRIM(str1)
Np1 = MAX(1,LEN_TRIM(pat1))
Np2 = LEN_TRIM(pat2)
Nk = 0
k = 0
DO n = 1,(Ns1-Np1+1)
    IF (str1(n:n+Np1-1)==pat1) THEN
        Nk = Nk + 1
        k(Nk) = n
    END IF
END DO
Ns2 = Ns1+Nk*(Np2-Np1)
ALLOCATE(CHARACTER(LEN=Ns2) :: str2)
IF (Nk==0) THEN
    str2 = str1
ELSE
    i1 = 1
    i2 = 1
    DO n = 1,Nk
        j1 = k(n)
        j2 = i2 + j1 - i1
        str2(i2:j2-1) = str1(i1:j1-1)
        str2(j2:j2+Np2-1) = pat2(1:Np2)
        i1 = j1 + Np1
        i2 = j2 + Np2
    END DO
    str2(i2:Ns2) = str1(i1:Ns1)
END IF

END FUNCTION strrep_all
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PURE FUNCTION strrep_1only(str1,pat1,pat2,back) RESULT(str2)
! REPLACE (WITHIN STR1) ANY INSTANCES OF PAT1 WITH PAT2
IMPLICIT NONE
! FUNCTION ARGUMENTS
CHARACTER(LEN=*),INTENT(IN) :: str1
CHARACTER(LEN=*),INTENT(IN) :: pat1
CHARACTER(LEN=*),INTENT(IN) :: pat2
LOGICAL,INTENT(IN) :: back
! FUNCTION RESULT
CHARACTER(LEN=:),ALLOCATABLE :: str2
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: k
INTEGER(KIND=ISGL) :: Ns1 ! NON-BLANK LENGTH OF STR1
INTEGER(KIND=ISGL) :: Ns2 ! NON-BLANK LENGTH OF STR2
INTEGER(KIND=ISGL) :: Np1 ! NON-BLANK LENGTH OF PAT1 (>=1)
INTEGER(KIND=ISGL) :: Np2 ! NON-BLANK LENGTH OF PAT2
CHARACTER(LEN=LEN(str1)) :: tmpstr

Np1 = MAX(1,LEN_TRIM(pat1))
Np2 = LEN_TRIM(pat2)
Ns1 = LEN_TRIM(str1)
k = strfind_1only(str1,pat1(1:Np1),back)
IF (k>0) THEN
    Ns2 = Ns1
    ALLOCATE(CHARACTER(LEN=Ns2) :: str2)
    str2 = str1
ELSE
    Ns2 = Ns1+Np2-Np1
    ALLOCATE(CHARACTER(LEN=Ns2) :: str2)
    str2(1:k-1) = str1(1:k-1)
    str2(k:k+Np2-1) = pat2(1:Np2)
    str2(k+Np2-1:Ns2) = str1(k+Np1-1:Ns1)
END IF

END FUNCTION strrep_1only
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE parse(str,delim,token,remain)
! RETURN THE PARTS OF A STRING PRECEDING (TOKEN) AND FOLLOWING (REMAIN) THE DELIM CHARACTER/S
! MULTIPLE DELIM OCCURRENCES ARE TREATED AS A SINGLE I.E. THEY ARE ALL STRIPPED

! SUBROUTINE ARGUMENTS
CHARACTER(LEN=*),INTENT(IN) :: str
CHARACTER(LEN=*),INTENT(IN) :: delim
CHARACTER(LEN=*),INTENT(OUT) :: token
CHARACTER(LEN=*),INTENT(OUT) :: remain
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: k
INTEGER(KIND=ISGL) :: Ns, Nd, i

token = str
remain = ''
Ns = LEN_TRIM(str)
IF (Ns==0) RETURN
k = strfind_1only(str,delim,.FALSE.)
IF (k==0) RETURN
Nd = MAX(1,LEN_TRIM(delim))
token = str(1:k-1)
remain = str(k+Nd:)
DO i = 1,LEN(remain)
    IF (remain(i:i+Nd-1)/=delim(1:Nd)) EXIT
    remain(i:i+Nd-1) = ''
END DO
remain = ADJUSTL(remain)

END SUBROUTINE parse
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PURE FUNCTION strmatch_all(strarr,pat) RESULT(k)
! SEARCH THROUGH STRARR AND FIND ANY ELEMENTS THAT START WITH THE STRING PAT
IMPLICIT NONE
! FUNCTION ARGUMENTS
CHARACTER(LEN=*),DIMENSION(:),INTENT(IN) :: strarr
CHARACTER(LEN=*),INTENT(IN) :: pat
! FUNCTION RESULT
INTEGER(KIND=ISGL),ALLOCATABLE,DIMENSION(:) :: k
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: i, Nk, Na, Ns, Np

Na = SIZE(strarr)
Ns = LEN(strarr)
Np = MAX(1,LEN_TRIM(pat))
IF ((Na==0).OR.(Ns==0).OR.(Np>Ns)) THEN
    Nk = 1
    ALLOCATE(k(Nk))
    k(Nk) = 0
    RETURN
END IF

Nk = 0
DO i = 1,Na
    IF (strarr(i)(1:Np)==pat(1:Np)) Nk = Nk + 1
END DO

IF (Nk==0) THEN
    Nk = 1
    ALLOCATE(k(Nk))
    k = 0
    RETURN
ELSE
    ALLOCATE(k(Nk))
    k = 0
END IF

Nk = 0
DO i = 1,Na
    IF (strarr(i)(1:Np)==pat(1:Np)) THEN
        Nk = Nk + 1
        k(Nk) = i
    END IF
END DO

END FUNCTION strmatch_all
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PURE FUNCTION strmatch_1only(strarr,pat,back) RESULT(k)
! SEARCH THROUGH STRARR AND FIND ANY ELEMENTS THAT START WITH THE STRING PAT
IMPLICIT NONE
! FUNCTION ARGUMENTS
CHARACTER(LEN=*),DIMENSION(:),INTENT(IN) :: strarr
CHARACTER(LEN=*),INTENT(IN) :: pat
LOGICAL,INTENT(IN) :: back
! FUNCTION RESULT
INTEGER(KIND=ISGL) :: k
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: i, Na, Ns, Np

k = 0
Na = SIZE(strarr)
Ns = LEN(strarr)
Np = MAX(1,LEN_TRIM(pat))
IF ((Na==0).OR.(Ns==0).OR.(Np>Ns)) RETURN

IF (.NOT.back) THEN
    DO i = 1,Na
        IF (strarr(i)(1:Np)==pat(1:Np)) THEN
            k = i
            EXIT
        END IF
    END DO
ELSE
    DO i = Na,1,-1
        IF (strarr(i)(1:Np)==pat(1:Np)) THEN
            k = i
            EXIT
        END IF
    END DO
END IF

END FUNCTION strmatch_1only
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ELEMENTAL FUNCTION width(num) RESULT(w)
! RETURN NUMBER OF DECIMALS IN AN INTEGER
IMPLICIT NONE
INTEGER(KIND=ISGL),INTENT(IN) :: num
! FUNCTION RESULT
INTEGER(KIND=ISGL) :: w

IF (num==0) THEN
    w = 1
ELSEIF (num>0) THEN
    w = AINT(LOG10(REAL(num)))+1
ELSE
    w = AINT(LOG10(REAL(ABS(num))))+2
END IF

END FUNCTION width
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PURE FUNCTION int2str_s(num) RESULT(str)
! RETURN A SCALAR INTEGER AS A STRING
IMPLICIT NONE
! FUNCTION ARGUMENTS
INTEGER(KIND=ISGL),INTENT(IN) :: num
! FUNCTION RESULT
CHARACTER(LEN=width(num)) :: str
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: w
CHARACTER(LEN=15) :: form, tmpstr

w = width(num)
WRITE(tmpstr, '(I12)') w
form = '(I'//TRIM(ADJUSTL(tmpstr))//')'
WRITE(str, TRIM(form)) num

END FUNCTION int2str_s
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PURE FUNCTION int2str_v(num) RESULT(str)
! RETURN A SCALAR INTEGER AS A STRING
IMPLICIT NONE
! FUNCTION ARGUMENTS
INTEGER(KIND=ISGL),DIMENSION(:),INTENT(IN) :: num
! FUNCTION RESULT
CHARACTER(LEN=SUM(width(num))+SIZE(num)-1) :: str
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: i, n, NN
INTEGER(KIND=ISGL) :: w
CHARACTER(LEN=15) :: form, tmpstr

NN = SIZE(num)
i = 1
DO n = 1,NN
    w = width(num(n))
    WRITE(tmpstr, '(I12)') w
    form = '(I'//TRIM(ADJUSTL(tmpstr))//')'
    WRITE(str(i:), TRIM(form)) num(n)
    i = i + w + 1
END DO

END FUNCTION int2str_v
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PURE FUNCTION real2str_s(num,fmt) RESULT(str)
! RETURN A SINGLE FLOAT SCALAR AS A STRING
IMPLICIT NONE
! FUNCTION ARGUMENTS
REAL(KIND=SGL),INTENT(IN) :: num
CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: fmt
! FUNCTION RESULT
CHARACTER(LEN=15) :: str
! LOCAL VARIABLES
CHARACTER(LEN=7) :: form

IF (PRESENT(fmt)) THEN
    form = fmt
ELSE
    form = '(G15.7)'
END IF

WRITE(str, form) num

END FUNCTION real2str_s
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PURE FUNCTION real2str_v(num,fmt) RESULT(str)
! RETURN A SINGLE FLOAT VECTOR AS A STRING
IMPLICIT NONE
! FUNCTION ARGUMENTS
REAL(KIND=SGL),DIMENSION(:),INTENT(IN) :: num
CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: fmt
! FUNCTION RESULT
CHARACTER(LEN=16*SIZE(num)-1) :: str
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: i, n, NN
CHARACTER(LEN=7) :: form

IF (PRESENT(fmt)) THEN
    form = fmt
ELSE
    form = '(G15.7)'
END IF

NN = SIZE(num)
i = 1
DO n = 1,NN
    WRITE(str(i:), form) num(n)
    i = i + 16
END DO

END FUNCTION real2str_v
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DEC$ IF (_PRECISION==1)
PURE FUNCTION dbl2str_s(num,fmt) RESULT(str)
! RETURN A DOUBLE FLOAT SCALAR AS A STRING
IMPLICIT NONE
! FUNCTION ARGUMENTS
REAL(KIND=DBL),INTENT(IN) :: num
CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: fmt
! FUNCTION RESULT
CHARACTER(LEN=15) :: str
! LOCAL VARIABLES
CHARACTER(LEN=7) :: form

IF (PRESENT(fmt)) THEN
    form = fmt
ELSE
    form = '(G15.7)'
END IF

WRITE(str, form) num

END FUNCTION dbl2str_s
!DEC$ END IF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DEC$ IF (_PRECISION==1)
PURE FUNCTION dbl2str_v(num,fmt) RESULT(str)
! RETURN A DOUBLE FLOAT VECTOR AS A STRING
IMPLICIT NONE
! FUNCTION ARGUMENTS
REAL(KIND=DBL),DIMENSION(:),INTENT(IN) :: num
CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: fmt
! FUNCTION RESULT
CHARACTER(LEN=16*SIZE(num)-1) :: str
! LOCAL VARIABLES
INTEGER(KIND=ISGL) :: i, n, NN
CHARACTER(LEN=7) :: form

IF (PRESENT(fmt)) THEN
    form = fmt
ELSE
    form = '(G15.7)'
END IF

NN = SIZE(num)
i = 1
DO n = 1,NN
    WRITE(str(i:), form) num(n)
    i = i + 16
END DO

END FUNCTION dbl2str_v
!DEC$ EN DIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE GEN_STRING