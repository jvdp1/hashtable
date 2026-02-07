SRCFYPP=\
	test_table_stream.fypp

SRCGEN = $(SRCFYPP:.fypp=.f90)

PROGS_SRC = test_hash.f90 test_table.f90 $(SRCGEN) #test_list.f90

NAME = hashtable

CPPFLAGS += -I../src

OBJS = $(PROGS_SRC:.f90=.o)
LIB = $(patsubst %, ../src/lib%.a, $(NAME))
PROGS = $(OBJS:.o=)
TESTPROGS = $(PROGS:=TEST)

.PHONY: all clean test $(TESTPROGS)

all: $(PROGS)

test: $(TESTPROGS)

$(TESTPROGS):
	./$(@:TEST=)

clean:
	$(RM) $(PROGS) $(OBJS) $(CLEAN_FILES) $(SRCGEN)

%.o: %.f90
	$(FC) $(FFLAGS) $(CPPFLAGS) -c $<

$(SRCGEN): %.f90: %.fypp ../src/common.fypp
	fypp $(FYPPFLAGS) $< $@

$(PROGS): %: %.o $(LIB)
	$(FC) $(FFLAGS) $(CPPFLAGS) -o $@ $^
