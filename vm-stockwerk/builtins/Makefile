CC     = g++
CFLAGS = -I../store -I../datalayer -DDEBUG_CHECK -ggdb #-DINTERFACE -m486 -finline-functions -fno-implement-inlines -O6

SRCS = Array.cc       \
       Char.cc        \
       CommandLine.cc \
       Future.cc      \
       General.cc     \
       Hole.cc        \
       Int.cc         \
       Math.cc        \
       OS.cc          \
       Real.cc        \
       String.cc      \
       TextIO.cc      \
       Thread.cc      \
       Unsafe.cc      \
       Vector.cc      \
       Word.cc        \
       Builtins.cc

OBJS = Array.o       \
       Char.o        \
       CommandLine.o \
       Future.o      \
       General.o     \
       Hole.o        \
       Int.o         \
       Math.o        \
       OS.o          \
       Real.o        \
       String.o      \
       TextIO.o      \
       Thread.o      \
       Unsafe.o      \
       Vector.o      \
       Word.o        \
       Builtins.o

all: $(OBJS)
	ld -shared -o libbuiltins.so $(OBJS)

$(OBJS): %.o : %.cc
	$(CC) $(CFLAGS) -c -o $@ $<

Array.o: Array.cc ../store/store.hh ../store/base.hh ../store/types.hh \
 ../store/headerdef.hh ../store/memchunk.hh ../store/dataset.hh \
 ../store/headerop.hh ../store/pointerop.hh ../store/value.hh \
 CommonOp.hh ListOp.hh Array.hh
Char.o: Char.cc ../store/store.hh ../store/base.hh ../store/types.hh \
 ../store/headerdef.hh ../store/memchunk.hh ../store/dataset.hh \
 ../store/headerop.hh ../store/pointerop.hh ../store/value.hh \
 CommonOp.hh Char.hh
CommandLine.o: CommandLine.cc ../store/store.hh ../store/base.hh \
 ../store/types.hh ../store/headerdef.hh ../store/memchunk.hh \
 ../store/dataset.hh ../store/headerop.hh ../store/pointerop.hh \
 ../store/value.hh CommonOp.hh CommandLine.hh
Future.o: Future.cc ../store/store.hh ../store/base.hh \
 ../store/types.hh ../store/headerdef.hh ../store/memchunk.hh \
 ../store/dataset.hh ../store/headerop.hh ../store/pointerop.hh \
 ../store/value.hh CommonOp.hh Future.hh
General.o: General.cc ../store/store.hh ../store/base.hh \
 ../store/types.hh ../store/headerdef.hh ../store/memchunk.hh \
 ../store/dataset.hh ../store/headerop.hh ../store/pointerop.hh \
 ../store/value.hh CommonOp.hh General.hh
Hole.o: Hole.cc ../store/store.hh ../store/base.hh ../store/types.hh \
 ../store/headerdef.hh ../store/memchunk.hh ../store/dataset.hh \
 ../store/headerop.hh ../store/pointerop.hh ../store/value.hh \
 CommonOp.hh Hole.hh
Int.o: Int.cc ../store/store.hh ../store/base.hh ../store/types.hh \
 ../store/headerdef.hh ../store/memchunk.hh ../store/dataset.hh \
 ../store/headerop.hh ../store/pointerop.hh ../store/value.hh \
 CommonOp.hh Int.hh
Math.o: Math.cc ../store/store.hh ../store/base.hh ../store/types.hh \
 ../store/headerdef.hh ../store/memchunk.hh ../store/dataset.hh \
 ../store/headerop.hh ../store/pointerop.hh ../store/value.hh \
 CommonOp.hh Math.hh
OS.o: OS.cc ../store/store.hh ../store/base.hh ../store/types.hh \
 ../store/headerdef.hh ../store/memchunk.hh ../store/dataset.hh \
 ../store/headerop.hh ../store/pointerop.hh ../store/value.hh \
 CommonOp.hh OS.hh
Real.o: Real.cc ../store/store.hh ../store/base.hh ../store/types.hh \
 ../store/headerdef.hh ../store/memchunk.hh ../store/dataset.hh \
 ../store/headerop.hh ../store/pointerop.hh ../store/value.hh \
 CommonOp.hh Real.hh
String.o: String.cc ../store/store.hh ../store/base.hh \
 ../store/types.hh ../store/headerdef.hh ../store/memchunk.hh \
 ../store/dataset.hh ../store/headerop.hh ../store/pointerop.hh \
 ../store/value.hh CommonOp.hh ListOp.hh String.hh
TextIO.o: TextIO.cc ../store/store.hh ../store/base.hh \
 ../store/types.hh ../store/headerdef.hh ../store/memchunk.hh \
 ../store/dataset.hh ../store/headerop.hh ../store/pointerop.hh \
 ../store/value.hh CommonOp.hh ListOp.hh TextIO.hh
Thread.o: Thread.cc ../store/store.hh ../store/base.hh \
 ../store/types.hh ../store/headerdef.hh ../store/memchunk.hh \
 ../store/dataset.hh ../store/headerop.hh ../store/pointerop.hh \
 ../store/value.hh CommonOp.hh Thread.hh
Unsafe.o: Unsafe.cc ../store/store.hh ../store/base.hh \
 ../store/types.hh ../store/headerdef.hh ../store/memchunk.hh \
 ../store/dataset.hh ../store/headerop.hh ../store/pointerop.hh \
 ../store/value.hh CommonOp.hh
Vector.o: Vector.cc ../store/store.hh ../store/base.hh \
 ../store/types.hh ../store/headerdef.hh ../store/memchunk.hh \
 ../store/dataset.hh ../store/headerop.hh ../store/pointerop.hh \
 ../store/value.hh CommonOp.hh ListOp.hh Vector.hh
Word.o: Word.cc ../store/store.hh ../store/base.hh ../store/types.hh \
 ../store/headerdef.hh ../store/memchunk.hh ../store/dataset.hh \
 ../store/headerop.hh ../store/pointerop.hh ../store/value.hh \
 CommonOp.hh Word.hh
Builtins.o: Builtins.cc ../store/store.hh ../store/base.hh ../store/types.hh \
 ../store/headerdef.hh ../store/memchunk.hh ../store/dataset.hh \
 ../store/headerop.hh ../store/pointerop.hh ../store/value.hh \
 CommonOp.hh Builtins.hh

clean:
	rm -rf $(OBJS) libbuiltins.so

distclean: clean
	rm -rf *~
