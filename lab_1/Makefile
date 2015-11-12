CC=gfortran
CFLAGS=-Wall -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -static-libgfortran -flto

all:
	$(CC) $(CFLAGS) -c src/environment.f90 -J obj/ -o obj/environment.o
	$(CC) $(CFLAGS) -c src/program.f90 -I obj/ -o obj/program.o
	$(CC) $(CFLAGS) -o bin/program -I obj/environment.o obj/program.o

clean:
	rm -f bin/*.*

run:
	cd ./bin; ./program;
	cat ./bin/output.txt
