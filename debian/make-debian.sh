#!/bin/bash

PKG=clsql
DEBPKG=cl-sql
PACKAGE_DIR=/usr/local/src/Packages/${DEBPKG}
WORK_DIR=/usr/local/src/Work/${PKG}

DEBVERSION=`sed -n -e "s/${DEBPKG} (\(.*\)).*/\1/p" < ${WORK_DIR}/debian/changelog  |head -1`

export CVSROOT=`cat CVS/Root`

echo "Building Debian files"
cvs-buildpackage -rfakeroot -kkevin@rosenberg.net -H${WORK_DIR}/debian/cvsbp-prepare.sh -F -d ${DEBPKG} -uc -us $*

rm -rf ${PACKAGE_DIR}/${DEBPKG}-${DEBVERSION}

if [ ! -z ${opt_lintian} ]; then
  pushd ${PACKAGE_DIR} > /dev/null
  lintian ${DEBPKG}_${DEBVERSION}_*.changes
  popd > /dev/null
fi

exit 0
