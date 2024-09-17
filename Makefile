# DESTINATION DIRECTORY 
DESTDIR ?= /Users/luigicrisci/Documents/Phd_Courses/Bloino/Project/Backup/Proj_copy/Exe
# FOTRAN COMPILER
FC=gfortran
# COMPILER OPTIONS
CPPFLAGS ?= -cpp
# PRROGRAM NAME
PROG = Dynamic_analyzer
PARALL ?= 
# OBJCECTS 
# SET DEF GOAL
all: $(PROG)
#PREPROCESSING VARIABLES
PREC ?=2
PRE_PRC_VAR= PREC=$(PREC)
# PATH VAR TO LIBRARY
IO_LIB =  $(PWD)/Libraries/IO_Dynamic_Lib
NUM_LIB = $(PWD)/Libraries/Numeric_Lib
ETC_LIB= $(PWD)/Libraries/Etc
MAIN_DIR=$(PWD)/Libraries/Main
# EXTRA FLAGS FOR FORTRAN COMPILER
FFLAGS = -O3 -fcheck=all

# VPATH FOR LIBRARY FILES
VPATH=$(IO_LIB):$(NUM_LIB):$(ETC_LIB):$(MAIN_DIR)


# DEFAULT GOAL: compile and link
all : $(PROG) 
# PHONY TARGETS
.PHONY: clean distclean uninstall
OBJCECTS= Kind_NumL_MOD.o Interfaces_EtcL_MOD.o Declaration_EtcL_MOD.o Parsing_EtcL_SUB.o Gnuplot_EtcL_SUB.o Up_to_low_EtcL_FUN.o ADMP_DecPreprocs_IodL_MOD.o\
   ADMP_INPUT_IodL_MOD.o ADMP_INPUT_IodL_SUBMOD.o ADMP_OUTPUT_IodL_MOD.o BinGen_IodL_SUBR.o Preprocs_NumL_MOD.o Interfaces_NumL_MOD.o RMSD_Proc_NumL.o Main.o



%.o : %.f08
	$(FC) $(FFLAGS) $(CPPFLAGS) -D $(PRE_PRC_VAR) $(PARALL) -c  $^ 

$(PROG) : $(OBJCECTS) 
	$(FC)  -o $@ $^


install: $(PROG)
	@if [ -z "$(DESTDIR)" ]; then \
		echo "Error: DESTDIR not specified"; \
		exit 1; \
	fi
	@echo "Installing $(PROG) to $(DESTDIR)"
# HANDLING FOR DESTDIR NON-EXISTANCE
	install -d $(DESTDIR) 
# PERMISSION TO BE AN EXECUTABLE 
	install -m 755 $(PROG) $(DESTDIR)/$(PROG) 


uninstall: 
	rm -f $(DESTDIR)/$(PROG)
		
# CLEAN RULE
clean:
	rm -f *.o *.smod *.mod 

distclean: clean
	rm -f $(PROG)


	
