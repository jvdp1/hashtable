SRC =\
	modhash.f90\
	modtable.f90

LIB = libhashtable.a

OBJS = $(SRC:.f90=.o)
MODS = $(OBJS:.o=.mod)

.PHONY: all clean

all: $(LIB)

$(LIB): $(OBJS)
	ar rcs $@ $(OBJS)

clean:
	$(RM) $(LIB) $(OBJS) $(MODS)

%.o: %.f90
	$(FC) $(FFLAGS) -c $<

%.f90: %.fypp
	fypp $(FYPPFLAGS) $< $@

#Fortran module dependencies
modtable.o: modhash.o

#Fortran sources bult from fypp templates
modtable.f90: modtable.fypp

