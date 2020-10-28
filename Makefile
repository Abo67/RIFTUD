FC            = gfortran	

FFLAGS	      = -g

DEST	      = $(HOME)/bin

EXTHDRS	      =

HDRS	      =

INSTALL	      = cp -v

LD	      = $(FC)

LDFLAGS	      = -lslatec  -llapack -lCoolProp

LIBS	      =

MAKEFILE      = Makefile

OBJS1         = contin.o \
		effic.o \
		pabc.o \
		riftud.o \
		shub.o \
		simps1.o

OBJS2         = contin.o \
		effic.o \
		pabc.o \
		riftudrg.o \
		shub.o \
		simps1.o \
		sonic_speed.o

PRINT	      = pr

PROG1       = riftud-pg

PROG2       = riftud-rg

SHELL	      = /bin/sh

SRCS	      = contin.f \
		effic.f \
		pabc.f \
		riftud.f \
		riftudrg.f \
		shub.f \
		simps1.f \
		sonic_speed.f

SYSHDRS	      =

all:		$(PROG1) $(PROG2)

$(PROG1):      	$(OBJS1) $(LIBS)
		@echo "Linking $(PROG1) ..."
		@$(LD) $(LDFLAGS) $(OBJS1) $(LIBS) -o $(PROG1)
		@echo "done"

$(PROG2):      	$(OBJS2) $(LIBS)
		@echo "Linking $(PROG2) ..."
		@$(LD) $(LDFLAGS) $(OBJS2) $(LIBS) -o $(PROG2)
		@echo "done"

clean:;		@rm -f $(OBJS1) $(OBJS2) core

clobber:;	@rm -f $(OBJS1) $(OBJS2) $(PROG1) $(PROG2) core tags

depend:;	@mkmf -f $(MAKEFILE)

echo:;		@echo $(HDRS) $(SRCS)

index:;		@ctags -wx $(HDRS) $(SRCS)

install:	$(PROG1) $(PROG2)
		@echo Installing $(PROG1) and $(PROG2) in $(DEST)
		@-strip $(PROG1) $(PROG2)
		@if [ $(DEST) != . ]; then \
		(rm -vf $(DEST)/$(PROG1) $(DEST)/$(PROG2); $(INSTALL) $(PROG1) $(PROG2) $(DEST)); fi

print:;		@$(PRINT) $(HDRS) $(SRCS)

tags:           $(HDRS) $(SRCS); @ctags $(HDRS) $(SRCS)
