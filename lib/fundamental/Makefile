SHELL		= /bin/sh
STOC		= stoc --no-implicit-import
STOC_OPTS	= 

TOPS		= RTT Pervasive ToplevelTypes ToplevelValues
SIGS		= ALT ARRAY BOOL CELL CHAR FUTURE GENERAL HOLE IEEE_REAL \
		  INTEGER LIST LIST_PAIR MATH OPTION PAIR PROMISE REAL REF \
		  STRING STRING_CVT SUBSTRING THREAD TIME VECTOR VECTOR_PAIR \
		  WORD
STRUCTS		= Alt Array Bool Cell Char Future General Hole Int \
		  LargeInt LargeReal LargeWord List ListPair Math Option Pair \
		  Position Promise Real Ref String StringCvt Substring Thread \
		  Time Toplevel Vector VectorPair WideChar WideString Word

SIGFILES	= $(SIGS:%=%-sig.ozf)
STRUCTFILES	= $(STRUCTS:%=%.ozf)
TOPFILES	= $(TOPS:%=%.ozf)
FILES		= $(TOPFILES) $(SIGFILES) $(STRUCTFILES)

.PHONY:			all install clean

all:			Fundamental.ozf

install:		all

clean:
			-rm *.ozf

%.ozf:			%.aml
			$(STOC) $(STOC_OPTS) -c $*.aml -o $@

RTT.aml:		RTTDummy.aml
			cp $< $@
Pervasive.ozf:		RTT.ozf
ToplevelTypes.ozf:	Pervasive.ozf
ToplevelValues.ozf:	Pervasive.ozf ToplevelTypes.ozf

$(SIGFILES):		Pervasive.ozf ToplevelTypes.ozf
$(STRUCTFILES):		Pervasive.ozf ToplevelTypes.ozf ToplevelValues.ozf

ALT-sig.ozf:		
Alt.ozf:		ALT-sig.ozf
ARRAY-sig.ozf:		
Array.ozf:		General.ozf List.ozf Vector.ozf ARRAY-sig.ozf
BOOL-sig.ozf:		
Bool.ozf:		BOOL-sig.ozf
CELL-sig.ozf:		
Cell.ozf:		CELL-sig.ozf
CHAR-sig.ozf:		
Char.ozf:		General.ozf Bool.ozf CHAR-sig.ozf
FUTURE-sig.ozf:		Time.ozf
Future.ozf:		Time.ozf Hole.ozf FUTURE-sig.ozf
GENERAL-sig.ozf:	
General.ozf:		GENERAL-sig.ozf
HOLE-sig.ozf:		
Hole.ozf:		HOLE-sig.ozf
IEEE_REAL-sig.ozf:	
IEEEReal.ozf:		IEEE_REAL-sig.ozf
INTEGER-sig.ozf:	StringCvt.ozf
Int.ozf:		Char.ozf StringCvt.ozf INTEGER-sig.ozf
LargeInt.ozf:		Int.ozf INTEGER-sig.ozf
LargeReal.ozf:		Real.ozf REAL-sig.ozf
LargeWord.ozf:		Word.ozf WORD-sig.ozf
LIST-sig.ozf:		
List.ozf:		General.ozf LIST-sig.ozf
LIST_PAIR-sig.ozf:	
ListPair.ozf:		List.ozf LIST_PAIR-sig.ozf
MATH-sig.ozf:		
Math.ozf:		Real.ozf MATH-sig.ozf
OPTION-sig.ozf:		
Option.ozf:		OPTION-sig.ozf
PAIR-sig.ozf:		
Pair.ozf:		PAIR-sig.ozf
Position.ozf:		Int.ozf INTEGER-sig.ozf
PROMISE-sig.ozf:	
Promise.ozf:		Cell.ozf Hole.ozf PROMISE-sig.ozf
REAL-sig.ozf:		IEEEReal.ozf StringCvt.ozf MATH-sig.ozf
Real.ozf:		IEEEReal.ozf Bool.ozf Int.ozf \
			Char.ozf StringCvt.ozf REAL-sig.ozf
REF-sig.ozf:		
Ref.ozf:		REF-sig.ozf
STRING-sig.ozf:		CHAR-sig.ozf
String.ozf:		Char.ozf Bool.ozf List.ozf STRING-sig.ozf
STRING_CVT-sig.ozf:	
StringCvt.ozf:		Char.ozf String.ozf STRING_CVT-sig.ozf
SUBSTRING.ozf:		STRING-sig.ozf
Substring.ozf:		SUBSTRING-sig.ozf
THREAD-sig.ozf:		Time.ozf
Thread.ozf:		Future.ozf Promise.ozf THREAD-sig.ozf
TIME-sig.ozf:		LargeInt.ozf LargeReal.ozf
Time.ozf:		LargeInt.ozf LargeReal.ozf IEEEReal.ozf TIME-sig.ozf
Toplevel.ozf:		General.ozf Bool.ozf Alt.ozf Option.ozf List.ozf \
			Real.ozf Char.ozf String.ozf Vector.ozf Future.ozf
VECTOR-sig.ozf:		
Vector.ozf:		General.ozf List.ozf VECTOR-sig.ozf
VECTOR_PAIR-sig.ozf:	
VectorPair.ozf:		General.ozf Int.ozf List.ozf Vector.ozf \
			VECTOR_PAIR-sig.ozf
WideChar.ozf:		Char.ozf CHAR-sig.ozf
WideString.ozf:		String.ozf STRING-sig.ozf
WORD.ozf:		StringCvt.ozf
Word.ozf:		LargeInt.ozf Char.ozf StringCvt.ozf WORD-sig.ozf

Fundamental.ozf:	$(FILES)
