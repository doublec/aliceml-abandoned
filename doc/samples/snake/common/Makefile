SOURCES = Color-prepare.aml Highscore-prepare.aml HIGHSCORE-sig.aml COLOR-sig.aml POS-sig.aml Pos.aml Protocol.aml Log.aml

ROOT = Main

all:	components

## adding some dependencies manually
Color.ozf:	Color-prepare.aml
Highscore.ozf:	Color.ozf Highscore-prepare.aml

-include ../AliceMakefile

