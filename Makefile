# FILE IDENTIFICATION
# 
#  Name:         Makefile
#  Purpose:      Makefile for the CLSQL package
#  Programer:    Kevin M. Rosenberg
#  Date Started: Mar 2002
#
#  CVS Id:   $Id: Makefile,v 1.21 2002/08/01 03:08:19 kevin Exp $
#
# This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
#
# CLSQL users are granted the rights to distribute and use this software
# as governed by the terms of the Lisp Lesser GNU Public License
# (http://opensource.franz.com/preamble.html), also known as the LLGPL.

PKG	:= clsql
DEBPKG	:= cl-sql
SUBDIRS	:= clsql clsql-uffi clsql-base clsql-mysql clsql-aodbc \
	   clsql-postgresql clsql-postgresql-socket 
DOCSUBDIRS:=doc

include Makefile.common

LIBSUBDIRS=interfaces/mysql interfaces/clsql-uffi
.PHONY: subdirs $(LIBSUBDIRS)

.PHONY: all
all: $(LIBSUBDIRS)

$(LIBSUBDIRS):
	$(MAKE) -C $@

.PHONY: distclean
distclean: clean
	@$(MAKE) -C doc $@

SOURCE_FILES=interfaces sql cmucl-compat doc test-suite Makefile VERSION \
	COPYING.CLSQL COPYING.MaiSQL README INSTALL ChangeLog NEWS TODO \
	set-logical.cl clsql-uffi.system \
	clsql.system clsql-aodbc.system clsql-mysql.system \
	clsql-postgresql.system clsql-postgresql-socket.system

.PHONY: doc
doc:
	$(MAKE) -C doc

.PHONY:dist
dist: clean
	@$(MAKE) -C doc $@


