## Author:
##   Andreas Rossberg <rossberg@ps.uni-sb.de>
## 
## Copyright:
##   Andreas Rossberg, 2001
## 
## Last change:
##   $Date$ by $Author$
##   $Revision$

# This makefile is for local testing purposes only,
# it is not required by the global build process.

SHELL		= /bin/sh
STOC		= stoc --implicit-import-file ../fundamental/Fundamental.import
STOC_OPTS	= 

SIGS		= NAME LABEL PATH FIXITY TYPE INF PERVASIVE_TYPE
STRUCTS		= Name Label Path PathSet PathMap Fixity Type Inf \
		  PervasiveType MkPervasiveType

SIGFILES	= $(SIGS:%=%-sig.ozf)
STRUCTFILES	= $(STRUCTS:%=%.ozf)
FILES		= $(SIGFILES) $(STRUCTFILES)

.PHONY:			all install clean

all:			RTT.ozf

install:		all

clean:
			-rm *.ozf *.ozp *.ozm

%.ozf:			%.aml
			$(STOC) $(STOC_OPTS) -c $*.aml -o $@

#TODO
Assert.ozf:		; cp ../utility/$@ .
GlobalStamp.ozf:	; cp ../utility/$@ .
LargeIntHashKey.ozf:	; cp ../utility/$@ .
StringHashKey.ozf:	; cp ../utility/$@ .
MkHashImpSet.ozf:	; cp ../utility/$@ .
MkHashImpMap.ozf:	; cp ../utility/$@ .

$(FILES):		../fundamental/Fundamental.ozf \
			../fundamental/Fundamental.import

NAME-sig.ozf:		
Name.ozf:		StringHashKey.ozf NAME-sig.ozf
LABEL-sig.ozf:		Name.ozf
Label.ozf:		Name.ozf LABEL-sig.ozf \
			LargeIntHashKey.ozf StringHashKey.ozf
FIXITY-sig.ozf:		
Fixity.ozf:		FIXITY-sig.ozf
PATH-sig.ozf:		Name.ozf Label.ozf
Path.ozf:		Name.ozf Label.ozf GlobalStamp.ozf PATH-sig.ozf
PathSet.ozf:		Path.ozf MkHashImpSet.ozf
PathMap.ozf:		Path.ozf MkHashImpMap.ozf
TYPE-sig.ozf:		Name.ozf Label.ozf Path.ozf PathSet.ozf PathMap.ozf
Type.ozf:		Name.ozf Label.ozf Path.ozf PathSet.ozf PathMap.ozf \
			Assert.ozf TYPE-sig.ozf
INF-sig.ozf:		Name.ozf Label.ozf Path.ozf PathMap.ozf Type.ozf \
			Fixity.ozf
Inf.ozf:		Name.ozf Label.ozf Path.ozf PathMap.ozf Type.ozf \
			Fixity.ozf Assert.ozf MkHashImpMap.ozf TYPE-sig.ozf
PERVASIVE_TYPE-sig.ozf:	Name.ozf Label.ozf Path.ozf Type.ozf
PervasiveType.ozf:	MkPervasiveType.ozf
MkPervasiveType.ozf:	Name.ozf Label.ozf Path.ozf Type.ozf \
			PERVASIVE_TYPE-sig.ozf

RTT.ozf:		$(FILES)
