.PHONY: all run build clean

all: run clean

run: build
	@./puzzle1
	@./puzzle2

build:
	@gfortran -O3 puzzle1.f90 -o puzzle1
	@gfortran -O3 puzzle2.f90 -o puzzle2

clean:
	@rm -rf puzzle1 puzzle2 mymodule.mod
