#!/bin/bash

PKG=clsql
DEBPKG=cl-sql

PACKAGE_DIR=/usr/local/src/Packages/${DEBPKG}
WORK_DIR=/usr/local/src/Work/${PKG}

echo "Building Debian files"
export CVSROOT=`cat CVS/Root`
cvs-buildpackage -rfakeroot -kkevin@rosenberg.net -F -d ${DEBPKG} -uc -us -sa -i.cvsignore $*

echo "Checking package with lintian"
DEBVERSION=`sed -n -e "s/${DEBPKG} (\(.*\)).*/\1/p" < ${WORK_DIR}/debian/changelog  |head -1`
pushd ${PACKAGE_DIR} > /dev/null
lintian ${DEBPKG}_${DEBVERSION}_*.changes
popd > /dev/null

exit 0
