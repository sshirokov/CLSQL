# FILE IDENTIFICATION
# 
#  Name:         Makefile
#  Purpose:      Makefile for the clsql documentation
#  Programer:    Kevin M. Rosenberg
#  Date Started: Mar 2002
#
#  CVS Id:   $Id: Makefile,v 1.15 2002/07/08 17:23:29 kevin Exp $
#
# This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
#
# CLSQL users are granted the rights to distribute and use this software
# as governed by the terms of the Lisp Lesser GNU Public License
# (http://opensource.franz.com/preamble.html), also known as the LLGPL.

# System variable to select catalog file
SYSTEM=debian
# SYSTEM=redhat

# Nothing to configure beyond this point

CATALOG=catalog.$(SYSTEM)

# Custom DSSSL's
DSSSL_HTML=../dsssl/html/docbook.dsl
DSSSL_PRINT=dsssl/print/docbook.dsl

DOCFILE_BASE_DEFAULT=clsql
DOCFILE_EXT_DEFAULT=sgml

# Standard docfile processing

ifndef DOCFILE_BASE
DOCFILE_BASE=${DOCFILE_BASE_DEFAULT}
endif

ifndef DOCFILE_EXT
DOCFILE_EXT=${DOCFILE_EXT_DEFAULT}
endif

DOCFILE=${DOCFILE_BASE}.${DOCFILE_EXT}
TEXFILE=${DOCFILE_BASE}.tex
PDFFILE=${DOCFILE_BASE}.pdf
PSFILE=${DOCFILE_BASE}.ps
DVIFILE=${DOCFILE_BASE}.dvi
TMPFILES=${DOCFILE_BASE}.aux ${DOCFILE_BASE}.out ${DOCFILE_BASE}.log
DOCFILES=$(shell echo *.sgml)

.PHONY: all
all: html pdf 

.PHONY: dist
dist: html pdf

.PHONY: doc
doc: html pdf

CHECK=nsgmls -s -C ${CATALOG} || exit 1

.PHONY: check
check:
	@$(CHECK)

.PHONY: html
html: html.tar.gz

html.tar.gz: $(DOCFILES) Makefile
	@$(CHECK)
	@( rm -rf html ; mkdir html; cd html ; jade -t sgml -c ../${CATALOG} -d ${DSSSL_HTML} ../${DOCFILE} > /dev/null; cp book1.html index.html; cd ..; GZIP='-9' tar czf html.tar.gz html)

.PHONY: tex
tex: ${TEXFILE}

${TEXFILE}: ${DOCFILES}
	@$(CHECK)
	@jade -t tex -c ${CATALOG} -d ${DSSSL_PRINT} ${DOCFILE} > /dev/null

.PHONY: pdf
pdf: ${PDFFILE}

${PDFFILE}: ${DOCFILES}
	@pdfjadetex -interaction=batchmode '\pdfcompresslevel=9' '\input ${TEXFILE}' > /dev/null
	@pdfjadetex -interaction=batchmode '\pdfcompresslevel=9' '\input ${TEXFILE}' > /dev/null
	@pdfjadetex -interaction=batchmode '\pdfcompresslevel=9' '\input ${TEXFILE}' > /dev/null
	@pdfjadetex -interaction=batchmode '\pdfcompresslevel=9' '\input ${TEXFILE}' > /dev/null

.PHONY: dvi
dvi: ${DVIFILE}

${DVIFILE}: ${TEXFILE}
	@jadetex ${TEXFILE} > /dev/null
	@jadetex ${TEXFILE} > /dev/null
	@jadetex ${TEXFILE} > /dev/null
	@jadetex ${TEXFILE} > /dev/null

.PHONY: ps
ps: ${PSFILE}

${PSFILE}: ${DVIFILE}
	@dvips -o ${PSFILE} ${DVIFILE} > /dev/null

.PHONY: clean
clean: 
	@rm -f *~ *.bak *.orig \#*\# .\#* 
	@rm -rf html ${PSFILE}
	@rm -f ${TMPFILES}
	@rm -f  ${DVIFILE} ${TEXFILE}

.PHONY: distclean
distclean: clean
