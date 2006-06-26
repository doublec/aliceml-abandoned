#
# Authors:
#   Gabor Szokoli <szokoli@ps.uni-sb.de>
#   Guido Tack <tack@ps.uni-sb.de>
#
# Copyright:
#   Gabor Szokoli and Guido Tack, 2004
#
#  See the file "../LICENSE" for information on usage and
#  redistribution of this file, and for a
#     DISCLAIMER OF ALL WARRANTIES.
#
# Last change:
#   $Date$ by $Author$
#   $Revision$
#

SOURCES = GecodeTest.aml TestSuite.aml RunTests.aml

all: $(SOURCES:%.aml=%.alc)

%.alc: %.aml
	alicec -c $< -o $@

depend:
	alicedep $(SOURCES) > Makefile.depend

-include Makefile.depend
