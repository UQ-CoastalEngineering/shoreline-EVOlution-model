
FC=ifort
SRCDIR=../../src

ifndef INSTALLDIR
  INSTALLDIR = /usr/local
endif

ifeq ($(COMPILATION_MODE),debug)
  FFLAGS=-g -traceback
  buildt=_debug
else
  ifeq ($(COMPILATION_MODE),profile)
    FFLAGS=-g -openmp -O2 -no-prec-div -fp-model fast=2 -axSSE4.1
    buildt=_prod
  else
    FFLAGS=-openmp -O2 -no-prec-div -fp-model fast=2 -axSSE4.1
    buildt=_prod
  endif
endif

ifeq ($(PRECISION),1)
  FFLAGS += -D_PRECISION=1
else ifeq ($(PRECISION),2)
  FFLAGS += -D_PRECISION=2
else
  FFLAGS += -D_PRECISION=1
endif

INCLUDES=\
-I$ ../../include\
-I$(NETCDFINCL)

LIBS=
LIBS += -L$(HDF5LIB) $(HDF5LIBNAME)
LIBS += -L$(NETCDFLIB) $(NETCDFLIBNAME)

OBJECTS=PRECISION.o ERROR.o GEN_STRING.o GEN_UTIL.o GEN_GEO.o LOG.o GEN_FILE.o\
globals.o EVOutils.o BCmod.o OUTmod.o WVmod.o XSmod.o LSmod.o EVOctrl.o EVOmod.o EVO.o

all: $(OBJECTS)
	$(FC) -o evomod $(FFLAGS) $(OBJECTS) $(LIBS)

%.o: $(SRCDIR)/%.f90
	$(FC) $(FFLAGS) $(INCLUDES) -c $<

%.o: $(SRCDIR)/LSmod/%.f90
	$(FC) $(FFLAGS) $(INCLUDES) -c $<
	
%.o: $(SRCDIR)/XSmod/%.f90
	$(FC) $(FFLAGS) $(INCLUDES) -c $<

%.o: $(SRCDIR)/WVmod/%.f90 
	$(FC) $(FFLAGS) $(INCLUDES) -c $<

%.o: $(SRCDIR)/generic/%.f90 
	$(FC) $(FFLAGS) $(INCLUDES) -c $<

clean:
	rm -f *.mod *.o evomod

