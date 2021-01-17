SRC =\
	modhash.f90\
	modlist.f90

LIB = liblist.a

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
modlist.o: modhash.o

#Fortran sources bult from fypp templates
modlist.f90: modlist.fypp

