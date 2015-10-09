CC=gfortran
CFLAGS=-Wall -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -static-libgfortran -flto
all:
	$(CC) $(CFLAGS) -o bin/program src/program.f90

clean:
	rm -f bin/*.*

run:
	cd ./bin; ./program;
	cat ./bin/output.txt
