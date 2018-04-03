! Generic Output Module

MODULE OUTmod

! MODULE USE STATEMENTS
USE PRECISION

! MODULE DECLARATIONS
IMPLICIT NONE

! MODULE ACCESS
PRIVATE
PUBLIC :: OutCtrlTyp, GenOutTyp

! MODULE TYPE DEFINITIONS

! OUTPUT CONTROL TYPE
TYPE OutCtrlTyp
    CHARACTER(LEN=MSTR) :: typnam                                   ! output type name
    CHARACTER(LEN=LSTR) :: fil                                      ! output file
    CHARACTER(LEN=LSTR) :: info                                     ! output information file (e.g. points file)
    REAL(KIND=DBL)  :: tstart                                       ! output start time
    REAL(KIND=DBL)  :: tfinal                                       ! output end time
    REAL(KIND=SGL) :: dt                                            ! output timestep
END TYPE
! GENERIC OBJECT SUB-TYPE
TYPE :: object_list
    INTEGER(KIND=ISGL),ALLOCATABLE,DIMENSION(:) :: id
END TYPE
! GENERIC 1D DATA SUB-TYPE
TYPE :: data_1d
    CHARACTER(LEN=MSTR) :: name
    REAL(KIND=DBL),ALLOCATABLE,DIMENSION(:) :: x
END TYPE
! GENERIC OUTPUT TYPE
TYPE,EXTENDS(OutCtrlTyp) :: GenOutTyp
    INTEGER(KIND=ISGL) :: typ                                       ! output type
    INTEGER(KIND=ISGL) :: fid                                       ! output file identifier
    LOGICAL :: init                                                 ! output initialised status
    REAL(KIND=DBL) :: t                                             ! next output time
    TYPE(object_list),ALLOCATABLE,DIMENSION(:) :: obj               ! output object lists
    TYPE(data_1d),ALLOCATABLE,DIMENSION(:) :: dat                   ! output data lists
END TYPE

END MODULE OUTmod