# FILE IDENTIFICATION
# 
#  Name:         Makefile
#  Purpose:      Makefile for the clsql documentation
#  Programer:    Kevin M. Rosenberg
#  Date Started: Mar 2002
#
#  CVS Id:   $Id: Makefile,v 1.6 2002/04/23 21:25:51 kevin Exp $
#
# This file, part of UFFI, is Copyright (c) 2002 by Kevin M. Rosenberg
#
# UFFI users are granted the rights to distribute and use this software
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

.PHONY: all dist check html tex pdf ps clean distclean

all: html pdf ps dvi

dist: html pdf

CHECK=nsgmls -s -C catalog || exit 1

check:
	$(CHECK)

html: html/book1.htm

html/book1.htm: ${DOCFILES} 
	$(CHECK)
	( rm -rf html ; mkdir html; cd html ; jade -t sgml -c ../catalog -d ${DSSSL_HTML} ../${DOCFILE}; cd ..)

tex: ${TEXFILE}

${TEXFILE}: ${DOCFILES}
	$(CHECK)
	@jade -t tex -c catalog -d ${DSSSL_PRINT} ${DOCFILE}

pdf: ${PDFFILE}

${PDFFILE}: ${TEXFILE}
	@pdfjadetex -interaction=batchmode '\pdfcompresslevel=9' '\input ${TEXFILE}' > /dev/null
	@pdfjadetex -interaction=batchmode '\pdfcompresslevel=9' '\input ${TEXFILE}' > /dev/null
	@pdfjadetex -interaction=batchmode '\pdfcompresslevel=9' '\input ${TEXFILE}' > /dev/null
	@pdfjadetex -interaction=batchmode '\pdfcompresslevel=9' '\input ${TEXFILE}' > /dev/null

dvi: ${DVIFILE}

${DVIFILE}: ${TEXFILE}
	jadetex ${TEXFILE}
	jadetex ${TEXFILE}
	jadetex ${TEXFILE}
	jadetex ${TEXFILE}

ps: ${PSFILE}

${PSFILE}: ${DVIFILE}
	dvips -o ${PSFILE} ${DVIFILE}

clean:
	@rm -rf html
	@rm -f ${PSFILE} ${PDFFILE} ${DVIFILE} ${TEXFILE}
	@rm -f ${TMPFILES}

realclean: clean
	@rm -f *~


