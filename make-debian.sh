#!/bin/bash

DEBPKG=cl-sql
DEBVERSION=`sed -n -e "s/${DEBPKG} (\(.*\)).*/\1/p" < debian/changelog  |head -1`
TOPDIR=`pwd`
PACKAGE_DIR=/usr/local/src/Packages/${DEBPKG}

export CVSROOT=`cat CVS/Root`

echo "Building Debian files"
cvs-buildpackage -rfakeroot -kkevin@rosenberg.net -H${TOPDIR}/cvsbp-prepare.sh -i.pdf -F -d ${DEBPKG} -uc -us $*

rm -rf ${PACKAGE_DIR}/${DEBPKG}-${DEBVERSION}

if [ ! -z ${opt_lintian} ]; then
  pushd ${PACKAGE_DIR} > /dev/null
  lintian ${DEBPKG}_${DEBVERSION}_*.changes
  popd > /dev/null
fi

exit 0
