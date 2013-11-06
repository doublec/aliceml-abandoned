###
# Makefile for Binding Generator
# Sven Woop


# executable names
ifdef WINDOWS
GENERATOR = Binding.x86-win32.exe
else
GENERATOR = Binding.x86-linux
endif

SML = sml


all: $(GENERATOR)

################### SOURCE CODE GENERTION #####################

$(GENERATOR):
	rm -f $(GENERATOR)
	echo 'CM.make(); compile();' | $(SML)
	test -e $(GENERATOR) || exit 1

########################## CLEANING ###########################

clean:
	rm -f $(GENERATOR)
