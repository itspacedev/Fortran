FC=gfortran
FFLAGS=-Wall -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -static-libgfortran -flto
FOPT=-O3 -fopt-info-vec

all:
	$(FC) $(FFLAGS) -c src/environment.f90 -J obj/ -o obj/environment.o
	$(FC) $(FFLAGS) -c src/program.f90 -I obj/ -o obj/program.o
	$(FC) $(FFLAGS) -o bin/program -I obj/environment.o obj/program.o

clean:
	rm -f obj/*
	rm -f bin/*

run:
	cd ./bin; ./program
	cat ./bin/output.txt
