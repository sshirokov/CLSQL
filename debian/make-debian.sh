#!/bin/bash

set -e

PKG=clsql
DEBPKG=cl-sql

BASE_HOME=/usr/local/src
WORK_HOME=${BASE_HOME}/Work
WORK_DIR=${WORK_HOME}/${PKG}
PACKAGE_DIR=${BASE_HOME}/Packages/${DEBPKG}
SOURCE_DIR=${WORK_HOME}/${PKG}

CHANGELOG=changelog
CHANGELOG=${WORK_DIR}/debian/changelog
UPSTREAM_VERSION=`sed -n -e "s/${DEBPKG} (\(.*\)-[0-9.]).*/\1/p" < ${CHANGELOG}  |head -1`
DEB_VERSION=`sed -n -e "s/${DEBPKG} (\(.*\)).*/\1/p" < ${CHANGELOG}  |head -1`

DEB_DIR=${WORK_HOME}/${PKG}-${UPSTREAM_VERSION}
ORIGINAL_TARBALL=${DEBPKG}_${UPSTREAM_VERSION}.orig.tar.gz
BUILD_LOG=${WORK_HOME}/${DEBPKG}_${DEB_VERSION}_build.log

if [ -z "${DEB_VERSION}" -o -z "${UPSTREAM_VERSION}" ]; then
  echo "Error: unable to parse version in changelog file"
  exit 1
fi

echo "Building Debian files"
test -s CVS/Root && export CVSROOT=`cat CVS/Root`
cd ${WORK_DIR}
cvs com -m 'Autocommit for make-debian'
cvs-buildpackage -rfakeroot -kkevin@rosenberg.net -F -d ${DEBPKG} -uc -us -sa -i -H${WORK_DIR}/debian/cvsbp-prepare.sh $* > ${BUILD_LOG}

echo "Checking package"
test -s ${PACKAGE_DIR}/${DEBPKG}_${DEBVERSION}_*.changes && \
    (cd ${PACKAGE_DIR}; \ 
    lintian ${DEBPKG}_${DEBVERSION}_*.changes)
