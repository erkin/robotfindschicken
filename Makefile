CC := csc

OPTS := -d0 -O2
BUILD_OPTS := -c -J
DEBUG_OPTS := -d2 -O0

CSC := $(CC) $(DEBUG_OPTS)

SRCDIR := src
BUILDDIR := build

TARGET := rfc

all: build clean

build: $(TARGET)

clean:
	rm -f $(BUILDDIR)/*.o
	rm -f *.import.scm

run: $(TARGET)
	./$(TARGET)


.PHONY: all build clean run static


$(BUILDDIR)/const.o: $(SRCDIR)/const.scm
	$(CSC) $(BUILD_OPTS) -o $(BUILDDIR)/const.o $(SRCDIR)/const.scm -unit const

$(BUILDDIR)/internal.o: $(SRCDIR)/internal.scm $(BUILDDIR)/const.o
	$(CSC) $(BUILD_OPTS) -o $(BUILDDIR)/internal.o $(SRCDIR)/internal.scm -unit internal -uses const

$(BUILDDIR)/draw.o: $(SRCDIR)/draw.scm $(BUILDDIR)/const.o $(BUILDDIR)/internal.o
	$(CSC) $(BUILD_OPTS) -o $(BUILDDIR)/draw.o $(SRCDIR)/draw.scm -unit draw -uses internal,const

$(BUILDDIR)/game.o: $(SRCDIR)/game.scm $(BUILDDIR)/draw.o $(BUILDDIR)/internal.o $(BUILDDIR)/const.o
	$(CSC) $(BUILD_OPTS) -o $(BUILDDIR)/game.o $(SRCDIR)/game.scm -unit game -uses draw,internal,const

$(TARGET): $(SRCDIR)/main.scm $(BUILDDIR)/game.o $(BUILDDIR)/draw.o $(BUILDDIR)/internal.o $(BUILDDIR)/const.o
	$(CSC) $^ -o $@ -uses game,draw,internal,const
