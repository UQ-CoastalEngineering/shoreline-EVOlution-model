
PROGRAM EVO

! USE MODULES
USE EVOmod
USE EVOutils
USE EVOctrl

! PARAMETERS
CHARACTER(LEN=3) :: model = 'EVO'
! OBJECTS
TYPE(EVOtyp) :: EV

! SET ERROR MODULE MODEL NAME
CALL set_model(model)

! OUTPUT BUILD DETAILS
CALL details()

! GET INPUT ARGUMENTS
CALL getargs()
CALL errchk(model)

! OPENING LOG FILE
CALL openlog(evcfull,path,evc)
CALL errchk(model)

! EXIT IF CONTROL FILENAME IS BLANK
IF (TRIM(evc)=='') THEN
    CALL cleanup()
    CALL errchk(model)
    STOP
END IF

! CONSTRUCT CONTROL OBJECT
CALL construct(ctrl,evcfil)
CALL errchk(model)

! INITIALISE GLOBALS
CALL initglobals(ctrl%global_ctrl_typ)

! CONSTRUCT EVO OBJECT
CALL construct(EV,ctrl)
CALL errchk(model)

! INITIALISE EVO OBJECT
CALL initialise(EV,ctrl)
CALL errchk(model)

! RUN EVO MODEL
CALL run(EV,ctrl)
CALL errchk(model)

! DESTRUCT EVO OBJECT
CALL destruct(EV)
CALL errchk(model)

! DESTRUCT CONTROL OBJECT
CALL destruct(ctrl)
CALL errchk(model)

! CLEANUP AND EXIT
CALL cleanup()
CALL errchk(model)

END PROGRAM EVO