##############################################################################
# FILE IDENTIFICATION
# 
#  Name:         Makefile
#  Purpose:      Makefile for the clsql documentation
#  Programer:    Kevin M. Rosenberg
#  Date Started: Mar 2002
#
#  CVS Id:   $Id$
#
# This file, part of CLSQL, is Copyright (c) 2002-2003 by Kevin M. Rosenberg
#
# CLSQL users are granted the rights to distribute and use this software
# as governed by the terms of the Lisp Lesser GNU Public License
# (http://opensource.franz.com/preamble.html), also known as the LLGPL.
##############################################################################

OS:=debian
DOCFILE_BASE_DEFAULT:=clsql
DOCFILE_EXT_DEFAULT:=xml

# Standard docfile processing

ifndef DOCFILE_BASE
DOCFILE_BASE=${DOCFILE_BASE_DEFAULT}
endif

ifndef DOCFILE_EXT
DOCFILE_EXT=${DOCFILE_EXT_DEFAULT}
endif

DOCFILE:=${DOCFILE_BASE}.${DOCFILE_EXT}
FOFILE:=${DOCFILE_BASE}.fo
PDFFILE:=${DOCFILE_BASE}.pdf
PSFILE:=${DOCFILE_BASE}.ps
DVIFILE:=${DOCFILE_BASE}.dvi
TXTFILE:=${DOCFILE_BASE}.txt
TMPFILES:=${DOCFILE_BASE}.aux ${DOCFILE_BASE}.out ${DOCFILE_BASE}.log
DOCFILES:=$(shell echo *.xml *.xsl)

CATALOG:=`pwd`/catalog-${OS}.xml
CHECK:=XML_CATALOG_FILES="$(CATALOG)" xmllint --noout --xinclude --postvalid $(DOCFILE) || exit 1

.PHONY: all
all: html pdf 

.PHONY: dist
dist: html pdf

.PHONY: doc
doc: html pdf

.PHONY: check
check:
	@$(CHECK)

.PHONY: html
html: html.tar.gz

html.tar.gz: $(DOCFILES) Makefile 
	@rm -rf html
	@mkdir html
	@XML_CATALOG_FILES="$(CATALOG)" xsltproc --xinclude --output html/ html.xsl $(DOCFILE)
	@GZIP='-9' tar czf html.tar.gz html 

.PHONY: fo
fo: ${FOFILE}

${FOFILE}: $(DOCFILES) Makefile 
	@XML_CATALOG_FILES="$(CATALOG)" xsltproc --xinclude --output $(FOFILE) fo.xsl $(DOCFILE)

.PHONY: pdf
pdf: ${PDFFILE}

${PDFFILE}: ${FOFILE}
	@fop $(FOFILE) -pdf $(PDFFILE) > /dev/null

.PHONY: dvi
dvi: ${DVIFILE}

.PHONY: ps
ps: ${PSFILE}

${PSFILE}: ${FOFILE}
	@fop $(FOFILE) -ps $(PSFILE) > /dev/null


.PHONY: txt
txt: ${TXTFILE}

${TXTFILE}: ${FOFILE}
	@fop $(FOFILE) -txt $(TXTFILE) > /dev/null

.PHONY: clean
clean: 
	@rm -f *~ *.bak *.orig \#*\# .\#* texput.log
	@rm -rf html ${PSFILE}
	@rm -f ${TMPFILES} ${FOFILE}
	@rm -f  ${DVIFILE} ${TXTFILE}

.PHONY: distclean
distclean: clean
