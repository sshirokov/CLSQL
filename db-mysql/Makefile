# -*- Mode: Makefile -*-
###########################################################################
# FILE IDENTIFICATION
# 
#  Name:         Makefile
#  Purpose:      Makefile for the CLSQL MySQL database interface
#  Programer:    Kevin M. Rosenberg
#  Date Started: Mar 2002
#
#  CVS Id:   $Id$
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
SHARED_LD_OPT=-shared
LD=gcc

# Use these for Sun's C compiler and Solaris (ACL)
#CC=cc
#SHARED_CC_OPT=-KPIC
#SHARED_LD_OPT=-G
#LD=ld

# May need to change to the directory where you have installed mysql's library
vpath %.so /usr/lib /usr/local/lib /opt/mysql/lib/mysql
vpath %.h /usr/include /usr/include/mysql /usr/local/include /usr/local/include/mysql /opt/mysql/include/mysql
INCLUDE_DIRS=-I/usr/include/mysql -I/usr/local/include/mysql -I/opt/mysql/include/mysql -I/usr/include -I/usr/local/include

# Nothing to configure beyond this point

base=clsql-mysql
source=$(base).c
object=$(base).o
shared_lib=$(base).so

.PHONY: all clean distclean

all: compile

compile: $(shared_lib)

$(object): $(source) mysql.h
	$(CC) ${SHARED_CC_OPT} $(INCLUDE_DIRS) -c $< -o $@

$(shared_lib): $(object) 
	$(LD) ${SHARED_LD_OPT} $^ -o $@

mac: $(object)
	ld -bundle /usr/lib/bundle1.o -flat_namespace -undefined suppress $(object) -o $(base).dylib
	ld -bundle /usr/lib/bundle1.o -flat_namespace -undefined suppress /sw/lib/libmysqlclient.dylib -o libmysqlclient.dylib
	ld -bundle /usr/lib/bundle1.o -flat_namespace -undefined suppress /usr/lib/libz.dylib -o libz.dylib

clean:
	@rm -f $(object) $(shared_lib) *~

distclean: clean

