PROGS_SRC = test_hash.f90 test_table.f90 #test_list.f90

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
	$(RM) $(PROGS) $(OBJS) $(CLEAN_FILES)

%.o: %.f90
	$(FC) $(FFLAGS) $(CPPFLAGS) -c $<

$(PROGS): %: %.o $(LIB)
	$(FC) $(FFLAGS) $(CPPFLAGS) -o $@ $^
