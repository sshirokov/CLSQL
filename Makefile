# FILE IDENTIFICATION
# 
#  Name:         Makefile
#  Purpose:      Makefile for the CLSQL package
#  Programer:    Kevin M. Rosenberg
#  Date Started: Mar 2002
#
#  CVS Id:   $Id: Makefile,v 1.23 2002/09/18 07:43:40 kevin Exp $
#
# This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
#
# CLSQL users are granted the rights to distribute and use this software
# as governed by the terms of the Lisp Lesser GNU Public License
# (http://opensource.franz.com/preamble.html), also known as the LLGPL.

PKG	:= clsql
DEBPKG	:= cl-sql
SUBDIRS	:= sql clsql-uffi base clsql-mysql clsql-aodbc \
	   clsql-postgresql clsql-postgresql-socket 
DOCSUBDIRS:=doc

include Makefile.common

LIBSUBDIRS=clsql-mysql clsql-uffi
.PHONY: subdirs $(LIBSUBDIRS)

.PHONY: all
all: $(LIBSUBDIRS)

$(LIBSUBDIRS):
	$(MAKE) -C $@

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


