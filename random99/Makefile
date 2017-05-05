F77=gfortran
#F77=ifort

SOURCES=$(wildcard *.f)
OBJECTS=$(SOURCES:.f=.o)

all : librandom99

librandom99 : $(OBJECTS)
	ar rvu librandom99.a $(OBJECTS)
	ranlib librandom99.a

%.o : %.f
	$(F77) -c $< -o $@

clean :
	rm -f *.o
