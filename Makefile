# FILE IDENTIFICATION
# 
#  Name:         Makefile
#  Purpose:      Makefile for the CLSQL package
#  Programer:    Kevin M. Rosenberg
#  Date Started: Mar 2002
#
#  CVS Id:   $Id: Makefile,v 1.8 2002/04/07 03:57:35 kevin Exp $
#
# This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
#
# CLSQL users are granted the rights to distribute and use this software
# as governed by the terms of the Lisp Lesser GNU Public License
# (http://opensource.franz.com/preamble.html), also known as the LLGPL.

PKG=clsql

all: libs

libs:
	(cd interfaces/mysql; make)
	(cd interfaces/clsql-uffi; make)

clean:
	@rm -f $(PKG)-*.tar.gz $(PKG)-*.zip
	@find . -type d -name .bin |xargs rm -rf 
	@find . -type f -name \*.a -or -name \*.so |xargs rm -rf 

realclean: clean
	@find . -type f -name "#*" -or -name \*~ -exec rm {} \;

docs:
	@(cd doc; make dist-doc)

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

tagcvs:
	cvs -q rtag -d $(TAG) $(PKG)
	cvs -q tag -F $(TAG)

dist: realclean docs tagcvs
	@rm -fr $(DISTDIR) $(DIST_TARBALL) $(DIST_ZIP)
	@mkdir $(DISTDIR)
	@cp -a $(SOURCE_FILES) $(DISTDIR)
	@find $(DISTDIR) -type d -name CVS | xargs rm -r
	@find $(DISTDIR) -type f -name .cvsignore -exec rm {} \;
	@find $(DISTDIR) -type f -and -name \*.tex -or -name \*.aux -or \
		 -name \*.log -or -name \*.out -or -name \*.dvi -or \
		 -name \*~ -or -name \*.ps -or -name test.config | xargs rm -f
	@tar czf $(DIST_TARBALL) $(DISTDIR)
	@find $(DISTDIR) -type f -name \*.cl -or -name \*.list -or \
		-name \*.system -or -name Makefile -or -name ChangeLog -or \
		-name COPYRIGHT -or -name TODO -or -name README -or -name INSTALL \
		-or -name NEWS -or -name \*.sgml -or -name COPYING\* -or -name catalog \
		| xargs unix2dos -q
	@zip -rq $(DIST_ZIP) $(DISTDIR)
	@rm -r $(DISTDIR)
