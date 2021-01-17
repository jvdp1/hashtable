SRCFYPP=\
	modtable.fypp

SRC =\
	modhash.f90\
	$(SRCGEN)


LIB = libhashtable.a

SRCGEN = $(SRCFYPP:.fypp=.f90)
OBJS = $(SRC:.f90=.o)
MODS = $(OBJS:.o=.mod)

.PHONY: all clean

all: $(LIB)

$(LIB): $(OBJS)
	ar rcs $@ $(OBJS)

clean:
	$(RM) $(LIB) $(OBJS) $(MODS) $(SRCGEN)

%.o: %.f90
	$(FC) $(FFLAGS) -c $<

$(SRCGEN): %.f90: %.fypp
	fypp $(FYPPFLAGS) $< $@

#Fortran module dependencies
modtable.o: modhash.o
