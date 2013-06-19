all: build
build: clean
	raco exe --gui calc.rkt
run:
	racket calc.rkt
clean:
	rm -f *~ calc calc.exe
	rm -rf calc.app
