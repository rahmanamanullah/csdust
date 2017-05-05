PYTHON=/usr/bin/python2

CC=gcc
CFLAGS=-O2 -Wall
CLIBS=-lm -Lrandom99 -l random99 -L/usr/lib -lgfortran -lcfitsio
#CLIBS=-lm

# gfortran
#
F77=gfortran
FOPT=-O3


# Analyze with gprof use the flag -pg
#
CFLAGS+= -pg
# FOPT+= -pg

# Intel's fortran compiler
#
#F77=ifort
#FOPT=-openmp -Wall
#FOPT=-O3 -xT -openmp

# Sad but true...
#
CERNLIB=-L/usr/lib64/cernlib/2006/lib/ -lpacklib -lmathlib -lkernlib
# CERNLIB=`cernlib`
FLIBS=-Lrandom99/ -lrandom99

all : scattercs cscattercs cscattercscvs

cscattercscvs : scattercs.cvs.c scattercs.h draine.o
	($(PYTHON) version.py)
	$(CC) $(CFLAGS) -o cscattercscvs scattercs.cvs.c draine.o $(CLIBS)

cscattercs : scattercs.c scattercs.h draine.o
	($(PYTHON) version.py)
	$(CC) $(CFLAGS) -o cscattercs scattercs.c draine.o $(CLIBS)

draine.o : draine.c draine.h
	$(CC) $(CFLAGS) -c -o draine.o draine.c

scattercs : scatter_colorcs_is.o random99/librandom99.a
	$(F77) $(FOPT) -o scattercs scatter_colorcs_is.o $(CERNLIB) $(FLIBS)

%.o : %.f
	$(F77) $(FOPT) -c $< -o $@

random99/librandom99.a : random99
	$(MAKE) -C random99

clean :
	$(MAKE) -C random99 clean
	rm -f *.o scattercs scattercs_LMC
