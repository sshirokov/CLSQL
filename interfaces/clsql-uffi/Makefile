# -*- Mode: Makefile -*-
###########################################################################
# FILE IDENTIFICATION
# 
#  Name:         Makefile
#  Purpose:      Makefile for the CLSQL UFFI helper package
#  Programer:    Kevin M. Rosenberg
#  Date Started: Mar 2002
#
#  CVS Id:   $Id: Makefile,v 1.4 2002/04/03 04:06:05 kevin Exp $
#
# This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
#
# CLSQL users are granted the rights to distribute and use this software
# as governed by the terms of the Lisp Lesser GNU Public License
# (http://opensource.franz.com/preamble.html), also known as the LLGPL.
###########################################################################


# These variables are correct for GCC
CC=gcc
SHARED_CC_OPT=-fPIC -DPIC

#SHARED_LD_OPT=-r # For FreeBSD and Solaris (CMUCL)
SHARED_LD_OPT=-Bshareable -Bdynamic  # For Linux (ALL) and FreeBSD (ACL)

# Use these for Sun's C compiler and Solaris (ACL)
#CC=cc
#SHARED_CC_OPT=-KPIC
#SHARED_LD_OPT=-G


# Nothing to configure beyond this point

base=clsql-uffi
source=$(base).c
object=$(base).o
shared_lib=$(base).so

all: $(shared_lib)

$(shared_lib): $(source) Makefile
	$(CC) ${SHARED_CC_OPT} -c $(source) -o $(object)
	ld ${SHARED_LD_OPT} $(object) -o $(shared_lib)
	rm $(object)

clean:
	rm -f $(object) $(shared_lib)

realclean: clean
	rm -f *~

