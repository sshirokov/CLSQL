#!/bin/bash 
#
# Creates upstream packages
# Programmer: Kevin Rosenberg

set -e # abort on error

usage () {
    progname="`basename \"$0\"`"

    cat >&2 <<EOF
Usage: $progname [options]
Creates upstream archives
Options:
  -c   Commit and tag CVS tree with current version numbers
  -t   Tag CVS tree with current version numbers
  -f   Force creation of upstream archive, even if exists'
  -h   Print this brief help
EOF
}

opt_force=0

# Command line
while [ $# != 0 ]; do
    value="`echo x\"$1\" | sed -e 's/^x-.//'`"
    case "$1" in
        -h)  usage; exit 0           ;;
        -c)  opt_commit=1; opt_tag=1 ;;
        -t)  opt_tag=1               ;;
	-f)  opt_force=1             ;;
         *)  usage; exit 0           ;;
    esac
    shift
done

DEBPKG=cl-sql
PKG=clsql
TOPDIR=`pwd`

VERSION=`sed -n -e "s/${DEBPKG} (\(.*\)-[0-9.]).*/\1/p" < debian/changelog  |head -1`

PACKAGE_DIR=/usr/local/src/Packages/${DEBPKG}
DISTDIR=${PKG}-${VERSION}
DEBDIR=${DEBPKG}-${VERSION}

if [ ! -z ${opt_commit} ]; then
    cvs commit -m 'Debian build'
fi

if [ ! -z ${opt_tag} ]; then
    UPSTREAM_TAG=upstream_version_`echo ${VERSION} | tr . _`
    echo "(Re-)tagging with Upstream tag '${UPSTREAM_TAG}'"
    cvs -q rtag -d $UPSTREAM_TAG $PKG > /dev/null
    cvs -q tag -F $UPSTREAM_TAG > /dev/null

fi

if [ -f ${PACKAGE_DIR}/${DEBPKG}_${VERSION}.orig.tar.gz ]; then
  echo "File ${PACKAGE_DIR}/${DEBPKG}_${VERSION}.orig.tar.gz already exists."
  echo -n "Are you sure that you want to create a new upstream archive? (y/N): "
  read answer
  case $answer in
      [Yy]*) nop= ;;
      *) echo "Not building"
	 exit 1
	  ;;
  esac
fi

# Prepare for archive
cd ..
rm -f ${PKG}_${VERSION}.tar.gz ${DEBPKG}_${VERSION}.orig.tar.gz
rm -rf ${DISTDIR} ${DEBDIR} ${DISTDIR}.zip
cp -a ${TOPDIR} ${DISTDIR}

echo "Cleaning distribution directory ${DISTDIR}"
cd ${DISTDIR}
rm -f upload.sh make-debian.sh make-upstream.sh cvsbp-prepare.sh test-suite/test.config
rm -f `find . -type f -name "*.so" -or -name "*.o"`
rm -f `find . -type f -name .cvsignore`
rm -rf `find . -type d -name CVS -or -name .bin`
rm -f `find . -type f -name '*~' -or -name '.#*'  -or -name '#*#' -or -name ".*~"`
rm -f `find doc -type f -name \*.tex -or -name \*.aux -or \
  -name \*.log -or -name \*.out -or -name \*.dvi`
cd ..

echo "Creating upstream archives"
rm -rf ${DISTDIR}/debian
GZIP=-9 tar czf ${DISTDIR}.tar.gz ${DISTDIR}

cp -a ${DISTDIR} ${DEBDIR}
GZIP=-9 tar czf ${DEBPKG}_${VERSION}.orig.tar.gz ${DEBDIR}

unix2dos `find ${DISTDIR} -type f -name \*.cl -or -name \*.list -or \
    -name \*.system -or -name Makefile -or -name ChangeLog -or \
    -name COPYRIGHT -or -name TODO -or -name README -or -name INSTALL -or \
    -name NEWS -or -name \*.sgml -or -name COPYING\* -or -name catalog`
zip -rq ${DISTDIR}.zip ${DISTDIR}

cp -a ${TOPDIR}/debian ${DEBDIR}
rm -f ${DEBDIR}/debian/.cvsignore 
rm -rf ${DEBDIR}/debian/CVS

rm -rf ${DISTDIR} ${DEBDIR}

echo "Moving upstream archives to ${PACKAGE_DIR}"
mkdir -p /usr/local/src/Packages/${DEBPKG}
rm -f ${PACKAGE_DIR}/${DISTDIR}.zip ${PACKAGE_DIR}/${DEBPKG}_${VERSION}.orig.tar.gz
mv ${DISTDIR}.zip ${DEBPKG}_${VERSION}.orig.tar.gz ${DISTDIR}.tar.gz ${PACKAGE_DIR}

cd ${TOPDIR}
exit 0
