CSC := csc -d2 -O0

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

$(BUILDDIR)/draw.o: $(SRCDIR)/draw.scm $(BUILDDIR)/const.o
	$(CSC) -c -J -o $(BUILDDIR)/draw.o $(SRCDIR)/draw.scm

$(BUILDDIR)/game.o: $(SRCDIR)/game.scm $(BUILDDIR)/draw.o $(BUILDDIR)/const.o
	$(CSC) -c -J -o $(BUILDDIR)/game.o $(SRCDIR)/game.scm

$(TARGET): $(SRCDIR)/main.scm $(BUILDDIR)/game.o $(BUILDDIR)/draw.o $(BUILDDIR)/const.o
	$(CSC) $^ -o $@
