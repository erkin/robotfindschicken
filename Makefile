PROJECT := rfc

EGGS := srfi-13 srfi-18 srfi-37 ncurses

CC := chicken-csc
AR := ar rc
SHELL := bash

OPTS := -d0 -O4 -v
DEBUG_OPTS := -d2 -O0
BUILD_OPTS := -c -J

CSC := $(CC) $(OPTS)

SRCDIR := src
BUILDDIR := build

TARGET := $(PROJECT)

.PHONY: all build clean run eggs

all: build clean

build: $(TARGET)
	strip $(TARGET)

clean:
	rm -f $(BUILDDIR)/*.{a,o,c}
	# csc emits import libraries in pwd
	rm -f *.import.scm
	rm -f $(TARGET).o

run: all
	./$(TARGET)

eggs:
	chicken-install -s $(EGGS)

# Build the object files
$(BUILDDIR)/const.o: $(SRCDIR)/const.scm
	$(CSC) $(BUILD_OPTS) -o $@ $(SRCDIR)/const.scm

$(BUILDDIR)/internal.o: $(SRCDIR)/internal.scm $(BUILDDIR)/const.o
	$(CSC) $(BUILD_OPTS) -o $@ $(SRCDIR)/internal.scm

$(BUILDDIR)/draw.o: $(SRCDIR)/draw.scm $(BUILDDIR)/internal.o
	$(CSC) $(BUILD_OPTS) -o $@ $(SRCDIR)/draw.scm

$(BUILDDIR)/game.o: $(SRCDIR)/game.scm $(BUILDDIR)/draw.o
	$(CSC) $(BUILD_OPTS) -o $@ $(SRCDIR)/game.scm

# Build executable
$(TARGET): $(SRCDIR)/main.scm $(BUILDDIR)/game.o $(BUILDDIR)/draw.o $(BUILDDIR)/internal.o $(BUILDDIR)/const.o
	$(CSC) $^ -o $@
