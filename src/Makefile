SRCFYPP=\
	modtable.fypp

SRC =\
	modhash.f90\
	$(SRCFYPP:.fypp=.f90)

LIB = libhashtable.a

OBJS = $(SRC:.f90=.o)
MODS = $(OBJS:.o=.mod)

.PHONY: all clean

all: $(LIB)

$(LIB): $(OBJS)
	ar rcs $@ $(OBJS)

clean:
	$(RM) $(LIB) $(OBJS) $(MODS) $(SRCFYPP:.fypp=.f90)

%.o: %.f90
	$(FC) $(FFLAGS) -c $<

%.f90: %.fypp
	fypp $(FYPPFLAGS) $< $@

#Fortran module dependencies
modtable.o: modhash.o
