ifdef SEAM
EXT:=alc
export EXT
endif

all:
	(cd common; make)
	(cd server; make)
ifdef SEAM
	(cd seam_client; make)
else
	(cd client; make)
endif

clean:
	(cd common; make clean)
	(cd server; make clean)
ifdef SEAM
	(cd seam_client; make)
else
	(cd client; make clean)
endif

