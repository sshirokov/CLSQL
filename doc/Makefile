# FILE IDENTIFICATION
# 
#  Name:         Makefile
#  Purpose:      Makefile for the clsql documentation
#  Programer:    Kevin M. Rosenberg
#  Date Started: Mar 2002
#
#  CVS Id:   $Id: Makefile,v 1.4 2002/03/26 20:51:41 kevin Exp $
#
# This file, part of UFFI, is Copyright (c) 2002 by Kevin M. Rosenberg
#
# UFFI users are granted the rights to distribute and use this software
# as governed by the terms of the Lisp Lesser GNU Public License
# (http://opensource.franz.com/preamble.html), also known as the LLGPL.



# Set to DSSSL
# For RedHat 6.x
#DSSSL_HTML=/usr/lib/sgml/stylesheets/nwalsh-modular/html/docbook.dsl
#DSSL_PRINT=/usr/lib/sgml/stylesheets/nwalsh-modular/print/docbook.dsl

# For RedHat 7.2
DSSSL_HTML=/usr/share/sgml/docbook/dsssl-stylesheets-1.64/html/docbook.dsl 
DSSSL_PRINT=/usr/share/sgml/docbook/dsssl-stylesheets-1.64/print/docbook.dsl

# Latest version
DSSSL_HTML=/usr/share/sgml/docbook/dsssl-stylesheets-1.76/html/docbook.dsl 
DSSSL_PRINT=/usr/share/sgml/docbook/dsssl-stylesheets-1.76/print/docbook.dsl

# Custom version
DSSSL_HTML=../dsssl/html/docbook.dsl
DSSSL_PRINT=dsssl/print/docbook.dsl

# Nothing to configure beyond this point

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

all: html pdf ps dvi

dist-doc: html pdf

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


