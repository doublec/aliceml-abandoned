SRCS = Primitives.oz Pickling.oz Emulator.oz

%.ozf: %.oz
	ozc -c $< -o $@

all: $(SRCS:%.oz=%.ozf)
