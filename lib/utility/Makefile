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

SIGS		= ASSERT STAMP GLOBAL_STAMP URL \
		  HASH_KEY IMP_SET IMP_MAP SCOPED_IMP_SET SCOPED_IMP_SET
STRUCTS		= Assert MkStamp GlobalStamp Url LargeIntHashKey StringHashKey \
		  MkHashImpSet MkHashImpMap MkScopedImpSet MkScopedImpMap

SIGFILES	= $(SIGS:%=%-sig.ozf)
STRUCTFILES	= $(STRUCTS:%=%.ozf)
FILES		= $(SIGFILES) $(STRUCTFILES)

.PHONY:			all install clean

all:			$(FILES)

install:		all

clean:
			-rm *.ozf

%.ozf:			%.aml
			$(STOC) $(STOC_OPTS) -c $*.aml -o $@

$(FILES):		../fundamental/Fundamental.ozf \
			../fundamental/Fundamental.import

ASSERT-sig.ozf:		
Assert.ozf:		ASSERT-sig.ozf
STAMP-sig.ozf:		
MkStamp.ozf:		STAMP-sig.ozf
GLOBAL_STAMP-sig.ozf:	STAMP-sig.ozf
GlobalStamp.ozf:	GLOBAL_STAMP-sig.ozf
URL-sig.ozf:		
Url.ozf:		Assert.ozf StringHashKey.ozf URL-sig.ozf
HASH_KEY-sig.ozf:	
LargeIntHashKey.ozf:	HASH_KEY-sig.ozf
StringHashKey.ozf:	HASH_KEY-sig.ozf
IMP_SET-sig.ozf:	
MkHashImpSet.ozf:	HASH_KEY-sig.ozf IMP_SET-sig.ozf
IMP_MAP-sig.ozf:	
MkHashImpMap.ozf:	HASH_KEY-sig.ozf IMP_MAP-sig.ozf
SCOPED_IMP_SET-sig.ozf:	
MkScopedImpSet.ozf:	IMP_SET-sig.ozf SCOPED_IMP_SET-sig.ozf
SCOPED_IMP_MAP-sig.ozf:	
MkScopedImpMap.ozf:	IMP_MAP-sig.ozf SCOPED_IMP_MAP-sig.ozf
#PRETTY_PRINT-sig.ozf:	
#PrettyPrint.ozf:	PRETTY_PRINT-sig.ozf
