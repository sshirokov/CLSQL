#!/bin/sh

# Creates debian and system-independent archive files
# Programmer: Kevin Rosenberg based on script used by onShore Development

set -e

VERSION=`cat VERSION`
DEBPKG=cl-sql
PKG=clsql
TOPDIR=`basename $PWD`

DISTDIR=${PKG}-${VERSION}
DEBDIR=${DEBPKG}-${VERSION}

TAG=upstream_version_`echo $VERSION | tr . _`
echo "(re)tagging with release tag '$TAG'"
cvs -q rtag -d $TAG $PKG
cvs -q tag -F $TAG


# build the tarball
echo "building tarballs"
cd ..
rm -f ${PKG}_${VERSION}.tar.gz ${DEBPKG}_${VERSION}.orig.tar.gz
rm -rf ${DISTDIR} ${DEBDIR} ${DISTDIR}.zip
cp -a ${TOPDIR} ${DISTDIR}

# Remove junk from distribution dir
find ${DISTDIR} -type f -name .cvsignore -exec rm {} \;
find ${DISTDIR} -type d -name CVS | xargs rm -r
find ${DISTDIR}/doc -type f -name \*.tex -or -name \*.aux -or \
    -name \*.log -or -name \*.out -or -name \*.dvi -or \
    -name \*~ -or -name .\#\*  -or -name \#*\# |xargs rm -f

# Copy dist dir to debian directory
cp -a ${DISTDIR} ${DEBDIR}
rm -f ${DEBDIR}/${PKG}.system
mv ${DEBDIR}/${PKG}.system.debian ${DEBDIR}/${PKG}.system

# Create original distribution archive
rm -rf ${DISTDIR}/debian ${DISTDIR}/*.system.debian 

GZIP=-9 tar czf ${DISTDIR}.tar.gz ${DISTDIR}

cp ${DISTDIR}.tar.gz ${DEBPKG}_${VERSION}.orig.tar.gz
find ${DISTDIR} -type f -and -name \*.cl -or -name \*.list -or \
    -name \*.system -or -name Makefile -or -name ChangeLog -or \
    -name COPYRIGHT -or -name TODO -or -name README -or -name INSTALL \
    -or -name NEWS -or -name \*.sgml -or -name COPYING\* -or -name catalog \
    | xargs unix2dos
zip -rq ${DISTDIR}.zip ${DISTDIR}


cp ${TOPDIR}/${PKG}.system.debian ${DEBDIR}
cd ${DEBDIR}
dpkg-buildpackage -rfakeroot -kkevin@b9.com

cd ..
rm -rf ${DISTDIR}
rm -rf ${DEBDIR}

lintian ${DEBPKG}_${VERSION}-*.changes

cd ${TOPDIR}
exit 0
