# FILE IDENTIFICATION
# 
#  Name:         Makefile
#  Purpose:      Makefile for the CLSQL package
#  Programer:    Kevin M. Rosenberg
#  Date Started: Mar 2002
#
#  CVS Id:   $Id: Makefile,v 1.19 2002/05/15 17:10:28 kevin Exp $
#
# This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
#
# CLSQL users are granted the rights to distribute and use this software
# as governed by the terms of the Lisp Lesser GNU Public License
# (http://opensource.franz.com/preamble.html), also known as the LLGPL.

PKG:=clsql
DEBPKG=cl-sql
SUBDIRS:=interfaces sql base
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

VERSION=$(shell cat VERSION)
DISTDIR=$(PKG)-$(VERSION)
DIST_TARBALL=$(DISTDIR).tar.gz
DIST_ZIP=$(DISTDIR).zip
SOURCE_FILES=interfaces sql cmucl-compat doc test-suite Makefile VERSION \
	COPYING.CLSQL COPYING.MaiSQL README INSTALL ChangeLog NEWS TODO \
	set-logical.cl clsql-uffi.system \
	clsql.system clsql-aodbc.system clsql-mysql.system \
	clsql-postgresql.system clsql-postgresql-socket.system

VERSION_UNDERSCORE=$(shell cat VERSION | tr . _)
TAG=dist_$(VERSION_UNDERSCORE)

.PHONY: doc
doc:
	$(MAKE) -C doc

.PHONY:dist
dist: clean
	@$(MAKE) -C doc $@

