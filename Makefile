# DESTINATION DIRECTORY (User defined)
DESTDIR ?=/Users/luigicrisci/Documents/Phd_Courses/Bloino/Project/Backup/Proj_copy/testsection/FinalTest
# IMMEDIATE EVALUATED VARIABLE THROUGH :=
DESTDIR := $(patsubst %/,%,$(DESTDIR))

# FORTRAN COMPILER
FC = gfortran

# COMPILER OPTIONS PREPROCESSING
CPPFLAGS ?= -cpp

# PROGRAM NAME
PROG =Dynamic_analyzer

# PARALLEL OPTIONS 
PARALL ?=

# DEFAULT GOAL
all: $(PROG)

# PREPROCESSING VARIABLES
PREC ?= 2
PRE_PRC_VAR = PREC=$(PREC)

# PATH VARIABLES TO LIBRARIES (Using $(CURDIR) for portability)
IO_LIB = $(CURDIR)/Libraries/IO_Dynamic_Lib
NUM_LIB = $(CURDIR)/Libraries/Numeric_Lib
ETC_LIB = $(CURDIR)/Libraries/Etc
MAIN_DIR = $(CURDIR)/Libraries/Main

# EXTRA FLAGS FOR FORTRAN COMPILER
FFLAGS = -O3 -fcheck=all -pedantic -Wall -Wno-maybe-uninitialized
# For the last key check the following 
# https://stackoverflow.com/questions/56261880/fortran-re-allocation-on-assignment-and-gfortran-warnings
# Define commands based on OS
ifeq ($(OS),Windows_NT)
    # Windows commands
    RM = del /Q
# MKDIR_P crea la directory solo se non esiste già (utile per evitare errori su Windows, ma da testare)
    MKDIR_P = if not exist "$(1)" mkdir "$(1)"
    CP = copy
    PATH_SEPARATOR = ;
    CHMOD = 
    EXECUTABLE = $(PROG).exe
else
    # Unix-like commands
    RM = rm -f
    MKDIR_P = mkdir -p
    CP = cp
    PATH_SEPARATOR = :
    CHMOD = chmod +x
    EXECUTABLE = $(PROG)
endif

# VPATH FOR LIBRARY FILES (Using PATH_SEPARATOR for portability)
VPATH = $(IO_LIB)$(PATH_SEPARATOR)$(NUM_LIB)$(PATH_SEPARATOR)$(ETC_LIB)$(PATH_SEPARATOR)$(MAIN_DIR)

# PHONY TARGETS
.PHONY: all clean distclean uninstall install

# OBJECTS
OBJECTS = Kind_NumL_MOD.o Declaration_EtcL_MOD.o Interfaces_EtcL_MOD.o  Parsing_EtcL_SUB.o \
          Gnuplot_EtcL_SUB.o Up_to_low_EtcL_FUN.o ADMP_DecPreprocs_IodL_MOD.o \
          ADMP_INPUT_IodL_MOD.o ADMP_INPUT_IodL_SUBMOD.o ADMP_OUTPUT_IodL_MOD.o \
          BinGen_IodL_SUBR.o Preprocs_NumL_MOD.o Interfaces_NumL_MOD.o RMSD_Proc_NumL.o Main.o

# Compilation rule
%.o : %.f08
	$(FC) $(FFLAGS) $(CPPFLAGS) -D$(PRE_PRC_VAR) $(PARALL) -c $<

# Linking rule
$(PROG): $(OBJECTS)
	@echo "Linking $(PROG)"
	$(FC) -o $(EXECUTABLE) $^

# Install rule
install: $(PROG)
	@echo "Installing $(EXECUTABLE) to $(DESTDIR)"
ifeq ($(OS),Windows_NT)
	$(call MKDIR_P,$(DESTDIR))
	$(CP) "$(EXECUTABLE)" "$(DESTDIR)\$(EXECUTABLE)"
else
	$(MKDIR_P) $(DESTDIR)
	$(CP) "$(EXECUTABLE)" "$(DESTDIR)/$(EXECUTABLE)"
	$(CHMOD) "$(DESTDIR)/$(EXECUTABLE)"
endif

# Uninstall rule
uninstall:
ifeq ($(OS),Windows_NT)
	$(RM) "$(DESTDIR)\$(EXECUTABLE)"
else
	$(RM) $(DESTDIR)/$(EXECUTABLE)
endif

# Clean rule
clean:
	-$(RM) *.o *.smod *.mod

# Distclean rule
distclean: clean
	-$(RM) $(EXECUTABLE)
	











# ++++++++++++++++++++++++++++++++++++++
#
#
#
#
# OLD VERSION, NON PORTABLE 
## DESTINATION DIRECTORY 
#DESTDIR ?= /Users/luigicrisci/Documents/Phd_Courses/Bloino/Project/Backup/Proj_copy/Exe
## FOTRAN COMPILER
#FC=gfortran
## COMPILER OPTIONS
#CPPFLAGS ?= -cpp
## PRROGRAM NAME
#PROG = Dynamic_analyzer
#PARALL ?= 
## OBJCECTS 
## SET DEF GOAL
#all: $(PROG)
##PREPROCESSING VARIABLES
#PREC ?=2
#PRE_PRC_VAR= PREC=$(PREC)
## PATH VAR TO LIBRARY
#IO_LIB =  $(PWD)/Libraries/IO_Dynamic_Lib
#NUM_LIB = $(PWD)/Libraries/Numeric_Lib
#ETC_LIB= $(PWD)/Libraries/Etc
#MAIN_DIR=$(PWD)/Libraries/Main
## EXTRA FLAGS FOR FORTRAN COMPILER
#FFLAGS = -O3 -fcheck=all

## VPATH FOR LIBRARY FILES
#VPATH=$(IO_LIB):$(NUM_LIB):$(ETC_LIB):$(MAIN_DIR)


## DEFAULT GOAL: compile and link
#all : $(PROG) 
## PHONY TARGETS
#.PHONY: clean distclean uninstall
#OBJCECTS= Kind_NumL_MOD.o Interfaces_EtcL_MOD.o Declaration_EtcL_MOD.o Parsing_EtcL_SUB.o Gnuplot_EtcL_SUB.o Up_to_low_EtcL_FUN.o ADMP_DecPreprocs_IodL_MOD.o\
#   ADMP_INPUT_IodL_MOD.o ADMP_INPUT_IodL_SUBMOD.o ADMP_OUTPUT_IodL_MOD.o BinGen_IodL_SUBR.o Preprocs_NumL_MOD.o Interfaces_NumL_MOD.o RMSD_Proc_NumL.o Main.o



#%.o : %.f08
#	$(FC) $(FFLAGS) $(CPPFLAGS) -D $(PRE_PRC_VAR) $(PARALL) -c  $^ 

#$(PROG) : $(OBJCECTS) 
#	$(FC)  -o $@ $^


#install: $(PROG)
#	@if [ -z "$(DESTDIR)" ]; then \
#		echo "Error: DESTDIR not specified"; \
#		exit 1; \
#	fi
#	@echo "Installing $(PROG) to $(DESTDIR)"
## HANDLING FOR DESTDIR NON-EXISTANCE
#	install -d $(DESTDIR) 
## PERMISSION TO BE AN EXECUTABLE 
#	install -m 755 $(PROG) $(DESTDIR)/$(PROG) 


#uninstall: 
#	rm -f $(DESTDIR)/$(PROG)
		
## CLEAN RULE
#clean:
#	rm -f *.o *.smod *.mod 

#distclean: clean
#	rm -f $(PROG)


	
