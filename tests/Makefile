SAGE = ../sage

LOGS = $(patsubst %.sage, %.out, $(wildcard *.sage))
NSLOGS = $(patsubst %.sage, %.nsout, $(wildcard *.sage))
BMLOGS = arith.out  bst.out heap.out mergesort.out \
         polylist.out printf.out regexp.out stlc.out
BMBASES = $(patsubst %.out, %, $(BMLOGS))
BMNSLOGS = $(patsubst %.out, %.nsout, $(BMLOGS))

FAULTLOGS = arith.fault heap.fault mergesort.fault orderedtree.fault \
	    polylist.fault printf.fault quicksort.fault regexp.fault

FAULTS = faults.tex

STATS = stats.tex

all: $(SAGE) $(LOGS)

nosimplify: $(SAGE) $(NSLOGS)

$(SAGE):
	cd ..; make

%.out: %.sage $(SAGE)
	@- ./test_harness.pl

%.nsout: %.sage $(SAGE)
	@- ./test_harness.pl

%.fault: %.sage $(SAGE)
	@- ../scripts/fault_injection  "$(SAGE)" $< | tee $@

hist: $(BMLOGS)
	../scripts/make_hist $(BMLOGS)

$(STATS): $(BMLOGS) $(BMNSLOGS) ../scripts/make_table.pl
	@ ../scripts/make_table.pl $(BMBASES) > $@

fault:	$(FAULTLOGS)
	grep "XXX" $(FAULTLOGS) > faults.tex

clean:
	rm -f $(LOGS) $(NSLOGS) $(FAULTLOGS) \
	statwrap.aux  statwrap.dvi statwrap.log stats.tex faults.tex \
	hist.dat histall.dat hist.ps default.db
