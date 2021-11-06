.PHONY: all run build clean

SOURCES = main.adb

all: run clean

run: build
	@./main

build: $(SOURCES)
	@gnatmake $(SOURCES)

clean:
	@gnatclean -q $(SOURCES)
