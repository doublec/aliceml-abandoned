NATIVES = AliceTimer

%.o: %.cc
	alicetool compile -c $< -o $@

%.dll: %.o
	alicetool link -Wl,-S $< -o $@

all: $(NATIVES:%=%.dll)

clean:
	rm -f $(NATIVES:%=%.o)

veryclean: clean
	rm -f $(NATIVES:%=%.dll.def)

distclean: veryclean
	rm -f $(NATIVES:%=%.dll)
