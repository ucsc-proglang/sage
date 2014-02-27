SAGE = ../sage
FFLAGS = -db ../cow.db -gcenv -nosimplify
LOGS = $(patsubst %.f, %.out, $(wildcard *.f))

all: $(LOGS) $(SAGE)

%.out: %.f
	@ ./refute_harness "$(SAGE) $(FFLAGS)" $< $@

clean:
	rm -f $(LOGS)
