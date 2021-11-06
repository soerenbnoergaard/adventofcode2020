.PHONY: all run build clean

all: run clean

run: build
	@./main_puzzle1
	@python3 main.py

build:
	@rustc main_puzzle1.rs -C opt-level=2

clean:
	@rm main_puzzle1
