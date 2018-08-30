CC := csc

OPTS := -d0 -O2
BUILD_OPTS := -c -J
DEBUG_OPTS := -d2 -O0

CSC := $(CC) $(OPTS)

AR := ar rc

SRCDIR := src
BUILDDIR := build

TARGET := rfc

all: build clean

build: $(TARGET)
	strip $(TARGET)

clean:
	rm -f $(BUILDDIR)/*
	rm -f *.import.scm

run: $(TARGET)
	./$(TARGET)

eggs:
	chicken-install -s ncurses

.PHONY: all build clean run eggs


$(BUILDDIR)/const.o: $(SRCDIR)/const.scm
	$(CSC) $(BUILD_OPTS) -o $(BUILDDIR)/const.o $(SRCDIR)/const.scm -unit const

$(BUILDDIR)/internal.o: $(SRCDIR)/internal.scm $(BUILDDIR)/const.o
	$(CSC) $(BUILD_OPTS) -o $(BUILDDIR)/internal.o $(SRCDIR)/internal.scm -unit internal -uses const

$(BUILDDIR)/draw.o: $(SRCDIR)/draw.scm $(BUILDDIR)/internal.o
	$(CSC) $(BUILD_OPTS) -o $(BUILDDIR)/draw.o $(SRCDIR)/draw.scm -unit draw -uses internal,const

$(BUILDDIR)/game.o: $(SRCDIR)/game.scm $(BUILDDIR)/draw.o
	$(CSC) $(BUILD_OPTS) -o $(BUILDDIR)/game.o $(SRCDIR)/game.scm -unit game -uses draw,internal,const

$(BUILDDIR)/objects.a: $(BUILDDIR)/game.o $(BUILDDIR)/draw.o $(BUILDDIR)/internal.o $(BUILDDIR)/const.o
	$(AR) $@ $^

$(TARGET): $(SRCDIR)/main.scm $(BUILDDIR)/objects.a
	$(CSC) $^ -o $@ -uses game,draw,internal,const
