# FILE IDENTIFICATION
# 
#  Name:         Makefile
#  Purpose:      Makefile for the CLSQL package
#  Programer:    Kevin M. Rosenberg
#  Date Started: Mar 2002
#
#  CVS Id:   $Id: Makefile,v 1.3 2002/03/27 08:16:24 kevin Exp $
#
# This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
#
# CLSQL users are granted the rights to distribute and use this software
# as governed by the terms of the Lisp Lesser GNU Public License
# (http://opensource.franz.com/preamble.html), also known as the LLGPL.

PACKAGE=clsql

all: libs

libs:
	(cd interfaces/mysql; make)
	(cd interfaces/clsql-uffi; make)

clean:
	@rm -f $(PACKAGE)-*.tar.gz $(PACKAGE)-*.zip
	@find . -type d -name .bin |xargs rm -rf 

realclean: clean
	@find . -type f -name \*~ -exec rm {} \;
	@find . -type f -name "#*#" -exec rm {} \;

docs:
	@(cd doc; make dist-doc)

VERSION=$(shell cat VERSION)
DISTDIR=$(PACKAGE)-$(VERSION)
DIST_TARBALL=$(DISTDIR).tar.gz
DIST_ZIP=$(DISTDIR).zip
SOURCE_FILES=interfaces sql cmucl-compat doc Makefile VERSION \
	COPYING.CLSQL COPYING.MaiSQL README INSTALL ChangeLog NEWS TODO \
	set-logical.cl test-clsql.cl \
	clsql.system clsql-aodbc.system clsql-mysql.system \
	clsql-postgresql.system clsql-postgresql-socket.system

dist: realclean docs
	@rm -fr $(DISTDIR) $(DIST_TARBALL) $(DIST_ZIP)
	@mkdir $(DISTDIR)
	@cp -a $(SOURCE_FILES) $(DISTDIR)
	@find $(DISTDIR) -type d -name CVS | xargs rm -r
	@find $(DISTDIR) -type f -name .cvsignore -exec rm {} \;
	@find $(DISTDIR)/doc -type f -name \*.tex -or -name \*.aux -or \
		 -name \*.log -or -name \*.out -or -name \*.dvi -or \
		 -name \*~ -or -name \*.ps -exec rm {} \;
	@tar czf $(DIST_TARBALL) $(DISTDIR)
	@find $(DISTDIR) -type f |grep -v .dll$ |grep -v .lib$ |xargs unix2dos -q
	@zip -rq $(DIST_ZIP) $(DISTDIR)
	@rm -r $(DISTDIR)
