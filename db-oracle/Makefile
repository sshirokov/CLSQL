# FILE IDENTIFICATION
#
#  Name:    Makefile
#  Purpose: Makefile for CLSQL Oracle interface
#  Author:  Kevin M. Rosenberg
#  Created: May 2004
#
#  CVS Id:   $Id$
#
# This file, part of CLSQL, is Copyright (c) 2004 by Kevin M. Rosenberg
#
# CLSQL users are granted the rights to distribute and use this software
# as governed by the terms of the Lisp Lesser GNU Public License
# (http://opensource.franz.com/preamble.html), also known as the LLGPL.

SUBDIRS=

include ../Makefile.common

base=oracle
shared_lib=$(base).so

.PHONY: all
all: $(shared_lib)

$(shared_lib): Makefile make.sh
	CFLAGS="" LDFLAGS="" BASE=$(base) OBJECT=$(object) SOURCE=$(source) SHARED_LIB=$(shared_lib) sh make.sh

.PHONY: distclean
distclean: clean
	rm -f $(base).so
