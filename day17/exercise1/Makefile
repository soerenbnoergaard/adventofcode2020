.PHONY: all run build clean

all: run clean

run: build
	@./main

build:
	@gfortran -O3 main.f90 -o main

clean:
	@rm -rf main mymodule.mod
