# -*- Mode: Makefile -*-
###########################################################################
# FILE IDENTIFICATION
# 
#  Name:         Makefile
#  Purpose:      Makefile for the CLSQL UFFI helper package
#  Programer:    Kevin M. Rosenberg
#  Date Started: Mar 2002
#
#  CVS Id:   $Id: Makefile,v 1.7 2002/04/07 08:48:54 kevin Exp $
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

# Set to the directory where you have installed mysql's library
#MYSQL_DIR=/opt/mysql
#MYSQL_LIB=$(MYSQL_DIR)/lib/mysql
MYSQL_DIR=/usr
MYSQL_LIB=$(MYSQL_DIR)/lib

MYSQL_LIB_FILE=$(MYSQL_LIB)/libmysqlclient.so
MYSQL_INCLUDE=$(MYSQL_DIR)/include/mysql


# Nothing to configure beyond this point

base=clsql-mysql
source=$(base).c
object=$(base).o
shared_lib=$(base).so

.PHONY: all clean distclean

all: $(shared_lib)

$(shared_lib): $(source) Makefile $(MYSQL_LIB_FILE)
	$(CC) ${SHARED_CC_OPT} -I $(MYSQL_INCLUDE) -c $(source) -o $(object)
	$(LD) ${SHARED_LD_OPT} $(object) $(MYSQL_LIB_FILE) -o $(shared_lib)
	@rm $(object)

clean:
	@rm -f $(object) $(shared_lib) *~

distclean: clean

