CC := csc
DEBUG_OPTS := -d2 -O0
OPTS := -d0 -O2

CSC := $(CC) $(OPTS)

TARGET := rfc

SRCDIR := src
BUILDDIR := build

all: build clean

build: $(TARGET)

clean:
	rm -f $(BUILDDIR)/*.o
	rm -f *.import.scm

run: $(TARGET)
	./$(TARGET)

.PHONY: all build clean run


$(BUILDDIR)/const.o: $(SRCDIR)/const.scm
	$(CSC) -c -J -o $(BUILDDIR)/const.o $(SRCDIR)/const.scm

$(BUILDDIR)/internal.o: $(SRCDIR)/internal.scm $(BUILDDIR)/const.o
	$(CSC) -c -J -o $(BUILDDIR)/internal.o $(SRCDIR)/internal.scm

$(BUILDDIR)/draw.o: $(SRCDIR)/draw.scm $(BUILDDIR)/const.o $(BUILDDIR)/internal.o
	$(CSC) -c -J -o $(BUILDDIR)/draw.o $(SRCDIR)/draw.scm

$(BUILDDIR)/game.o: $(SRCDIR)/game.scm $(BUILDDIR)/draw.o $(BUILDDIR)/internal.o $(BUILDDIR)/const.o
	$(CSC) -c -J -o $(BUILDDIR)/game.o $(SRCDIR)/game.scm

$(TARGET): $(SRCDIR)/main.scm $(BUILDDIR)/game.o $(BUILDDIR)/draw.o $(BUILDDIR)/internal.o $(BUILDDIR)/const.o
	$(CSC) $^ -o $@
