MORE = -I ~/devel/stockhausen/install/include
CC = gcc `pkg-config --cflags --libs gtk+-2.0 libgnomecanvas-2.0` $(MORE)

all: test canvastest

%: %.cc
	$(CC) $< -o $@
