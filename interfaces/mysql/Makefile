# -*- Mode: Makefile -*-
###########################################################################
# FILE IDENTIFICATION
# 
#  Name:         Makefile
#  Purpose:      Makefile for the CLSQL UFFI helper package
#  Programer:    Kevin M. Rosenberg
#  Date Started: Mar 2002
#
#  CVS Id:   $Id: Makefile,v 1.2 2002/04/01 05:27:55 kevin Exp $
#
# This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
#
# CLSQL users are granted the rights to distribute and use this software
# as governed by the terms of the Lisp Lesser GNU Public License
# (http://opensource.franz.com/preamble.html), also known as the LLGPL.
###########################################################################

AR=ar

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

# Set to the directory where you have installed mysql's library
#MYSQL_DIR=/usr
MYSQL_DIR=/opt/mysql

MYSQL_LIB=$(MYSQL_DIR)/lib/mysql
MYSQL_LIB_FILE=$(MYSQL_LIB)/libmysqlclient.so
MYSQL_INCLUDE=$(MYSQL_DIR)/include/mysql

# Nothing to configure beyond this point

BASE=clsql-mysql
SRC=$(BASE).c
OBJECT=$(BASE).o
SHARED=$(BASE).so
STATIC=$(BASE).a

all: $(SHARED) $(STATIC)

$(SHARED): $(SRC) $(MYSQL_LIB_FILE)
	$(CC) $(SHARED_CC_OPT) -I $(MYSQL_INCLUDE) -c $(SRC) -o $(OBJECT)
	ld $(SHARED_LD_OPT) $(OBJECT) $(MYSQL_LIB_FILE) -o $(SHARED)
	@rm $(OBJECT)

$(STATIC): $(SRC) $(MYSQL_LIB_FILE)
	$(CC) -I $(MYSQL_INCLUDE) -c $(SRC) -o $(OBJECT)
	$(AR) r $(STATIC) $(OBJECT)
	@rm $(OBJECT)

clean:
	rm -f $(SHARED) $(STATIC)

realclean: clean
	rm -f *~

