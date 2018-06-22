#!/bin/bash

source /opt/intel/Compiler/11.1/069/bin/ifortvars.sh intel64
source /opt/intel/Compiler/11.1/069/bin/iccvars.sh intel64

export PRECISION=1
export NETCDFINCL=/usr/local/include
export NETCDFLIB=/usr/local/lib
export NETCDFLIBNAME="-lnetcdff -lnetcdf"
export HDF5LIB=/usr/local/hdf5-1.8.8/lib
export HDF5LIBNAME="-lhdf5_hl -lhdf5"
#export COMPILATION_MODE=production
export COMPILATION_MODE=debug
export FORTRAN_COMPILER=ifort
cd ./platform/linux_ifort
env
make clean
make
rm -f *.mod *.o
