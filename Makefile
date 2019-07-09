PROJECT := rfc

EGGS := ncurses srfi-18

CC := chicken-csc
AR := ar rc
SHELL := bash

OPTS := -d0 -O2 -v
DEBUG_OPTS := -d2 -O0
BUILD_OPTS := -c -J

CSC := $(CC) $(OPTS)

SRCDIR := src
BUILDDIR := build

TARGET := $(PROJECT)

# For building the AppImage only
PREFIX := $(BUILDDIR)/usr
APP := robotfindschicken.AppImage


.PHONY: all build clean run eggs app app_build app_clean app_run


## Building normally

all: build clean

build: $(TARGET)
	strip $(TARGET)

clean:
	rm -f $(BUILDDIR)/*.{a,o,c}
	# csc emits import libraries in pwd
	rm -f *.import.scm

run: all
	./$(TARGET)

eggs:
	chicken-install -s ncurses

## Building AppImage

app: $(APP) app_clean

app_build: $(BUILDDIR)/$(PROJECT)
	mv $^/{$(PROJECT),libchicken.so.11,ncurses.so} $(PREFIX)/lib/
	strip $(PREFIX)/lib/*
	rm -rf $^

app_clean: clean
	rm -rf $(PREFIX)
	rm -rf $(BUILDDIR)/$(PROJECT)
	rm -rf $(PROJECT).AppDir

app_run: app
	./$(APP)


# Build the object files
$(BUILDDIR)/const.o: $(SRCDIR)/const.scm
	$(CSC) $(BUILD_OPTS) -o $@ $(SRCDIR)/const.scm -unit const

$(BUILDDIR)/internal.o: $(SRCDIR)/internal.scm $(BUILDDIR)/const.o
	$(CSC) $(BUILD_OPTS) -o $@ $(SRCDIR)/internal.scm -unit internal -uses const

$(BUILDDIR)/draw.o: $(SRCDIR)/draw.scm $(BUILDDIR)/internal.o
	$(CSC) $(BUILD_OPTS) -o $@ $(SRCDIR)/draw.scm -unit draw -uses internal,const

$(BUILDDIR)/game.o: $(SRCDIR)/game.scm $(BUILDDIR)/draw.o
	$(CSC) $(BUILD_OPTS) -o $@ $(SRCDIR)/game.scm -unit game -uses draw,internal,const

$(BUILDDIR)/objects.a: $(BUILDDIR)/game.o $(BUILDDIR)/draw.o $(BUILDDIR)/internal.o $(BUILDDIR)/const.o
	$(AR) $@ $^


# Build executable
$(TARGET): $(SRCDIR)/main.scm $(BUILDDIR)/objects.a
	$(CSC) $^ -o $@ -uses game,draw,internal,const

# Build the app
$(BUILDDIR)/$(PROJECT): $(SRCDIR)/main.scm $(BUILDDIR)/objects.a
	mkdir -p $(PREFIX)/{lib,bin,share}
	$(CSC) -deploy $^ -o $@ -uses game,draw,internal,const
	chicken-install -deploy $(EGGS) -p $@

# Pack AppDir and generate AppImage
$(APP): app_build
	cp -r assets/licence-info $(PREFIX)/share/doc
	mkdir -p $(PROJECT).AppDir
	cp assets/{chikun.png,RFC.desktop} $(PROJECT).AppDir
	# TODO: A cleaner way to do this
	ln -s ../lib/$(PROJECT) $(PREFIX)/bin/$(PROJECT)
	cp -r $(PREFIX) $(PROJECT).AppDir
	# TODO: and this
	ln -s usr/bin/$(PROJECT) $(PROJECT).AppDir/AppRun
	appimagetool $(PROJECT).AppDir $@
	chmod +x $@


# Local Variables:
#   mode: makefile
#   indent-tabs-mode: t
# End:
