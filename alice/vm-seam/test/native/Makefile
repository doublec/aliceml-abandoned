NATIVES = Timer

%.o: %.cc
	alicetool -v cc -c $< -o $@

%.dll: %.o
	alicetool -v ld -Wl,-S $< -o $@

all: $(NATIVES:%=%.dll)

clean:
	rm -f $(NATIVES:%=%.o)

veryclean: clean
	rm -f $(NATIVES:%=%.dll.def)

distclean: veryclean
	rm -f $(NATIVES:%=%.dll)
