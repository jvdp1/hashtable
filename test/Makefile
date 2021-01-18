PROGS_SRC = test_hash.f90 test_table.f90# test_list.f90

CPPFLAGS += -I../src
LDFLAGS += -L../src -lhashtable

OBJS = $(PROGS_SRC:.f90=.o)
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

$(PROGS): %: %.o
	$(FC) $(FFLAGS) $(CPPFLAGS) -o $@ $^ $(LDFLAGS)
