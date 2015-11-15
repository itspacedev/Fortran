FC=gfortran
FFLAGS=-Wall -fall-intrinsics -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -Wunused-variable -static-libgfortran -flto
FOPT=-O3 -fopt-info-vec

all:
	$(FC) $(FFLAGS) -c src/environment.f90 -J obj/ -o obj/environment.o 
	$(FC) $(FFLAGS) -c src/group_io.f90 -J obj/ -o obj/group_io.o
	$(FC) $(FFLAGS) -c src/group_process.f90 -J obj/ -o obj/group_process.o
	$(FC) $(FFLAGS) -c src/program.f90 -I obj/ -o obj/program.o
	$(FC) $(FFLAGS) -o bin/program ./obj/environment.o obj/group_io.o obj/group_process.o obj/program.o

clean:
	rm -f obj/*
	rm -f bin/*

run:
	cd ./bin; ./program
	cat ./bin/output.txt
