# Fortran stdlib Makefile

FC = ifort
FFLAGS = -g -check all -traceback -stand f08
#FC = gfortran
#FFLAGS = -g -Wall -std=f2008
FYPPFLAGS=

export FC
export FFLAGS
export FYPPFLAGS

.PHONY: all clean test

all:
	$(MAKE) --directory=src
	$(MAKE) --directory=test

test:
	$(MAKE) --directory=test test
	@echo
	@echo "All tests passed."

clean:
	$(MAKE) clean --directory=src
	$(MAKE) clean --directory=test
