OZC = ozc
OZCFLAGS =

FUNCTORS = Intermediate FrontEnd Info Predefined Annotate

all: $(FUNCTORS:%=%.ozf)

%.ozf: %.oz
	$(OZC) -c $< -o $@

clean:
	-rm -f $(FUNCTORS:%=%.ozf)

veryclean: clean

distclean: veryclean
