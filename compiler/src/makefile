
## -----------------------------------------------------------------------1- ##
## This file is part of the FM3 Modula-3 compiler.                           ##
## Copyright 2023        Rodney M. Bates.                                    ##
## rodney.m.bates@acm.org                                                    ##
## Licensed under the MIT License.                                           ##
## -----------------------------------------------------------------------2- ##

# makefile for compiler/src. 

DATE = date
SED = sed

all : ForceVersion ../derived/FM3Version.i3

mkderived:
	sh -c "if test ! -d ../derived; then mkdir ../derived; else true; fi"

ForceVersion : 
	rm -f ../derived/FM3Version.i3

../derived/FM3Version.i3 : FM3Version.i30
	$(SED) -e "s/%DATESTRING%/`$(DATE)`/" FM3Version.i30 > ../derived/FM3Version.i3
