# FILE IDENTIFICATION
#
#  Name:         Makefile
#  Purpose:      Makefile for CLSQL MySQL interface
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

SUBDIRS=

include ../Makefile.common

base=mysql
source=$(base).c
object=$(base).o
shared_lib=$(base).so

.PHONY: all
all: $(shared_lib)

$(shared_lib): $(source) Makefile
	CFLAGS="-I /usr/include/mysql -I /usr/local/include/mysql" LDFLAGS="-L/usr/local/lib/mysql -lmysqlclient" BASE=$(base) OBJECT=$(object) SOURCE=$(source) SHARED_LIB=$(shared_lib) sh make.sh
	rm $(object)

.PHONY: distclean
distclean: clean
	rm -f $(base).dylib $(base).dylib $(base).so $(base).o
