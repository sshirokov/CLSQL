# -*- Mode: Makefile -*-
###########################################################################
# FILE IDENTIFICATION
# 
#  Name:         Makefile
#  Purpose:      Makefile for the CLSQL UFFI helper package
#  Programer:    Kevin M. Rosenberg
#  Date Started: Mar 2002
#
#  CVS Id:   $Id: Makefile,v 1.1 2002/03/27 07:58:42 kevin Exp $
#
# This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
#
# CLSQL users are granted the rights to distribute and use this software
# as governed by the terms of the Lisp Lesser GNU Public License
# (http://opensource.franz.com/preamble.html), also known as the LLGPL.
###########################################################################


# These variables are correct for GCC
# you'll need to modify these for other compilers
CC=gcc
SHARED_CC_OPT=-fpic
SHARED_LD_OPT=-shared

# If you are using Solaris, these are the correct values
# for creating a shared library
#CC=cc
#SHARED_CC_OPT=-KPIC
#SHARED_LD_OPT=-G

# Nothing to configure beyond this point

BASE=clsql-uffi
SRC=${BASE}.c
OBJECT=${BASE}.o
LIB=${BASE}.so

all: ${LIB}

${LIB}: ${SRC} 
	${CC} ${SHARED_CC_OPT} -c ${SRC} -o ${OBJECT}
	ld ${SHARED_LD_OPT} ${OBJECT} -o ${LIB}
	@rm ${OBJECT}

clean:
	rm -f ${LIB}

realclean: clean
	rm -f *~

