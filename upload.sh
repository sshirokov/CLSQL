#!/bin/sh

PKG=clsql
DEBPKG=cl-sql

set -e # abort on error

usage () {
    progname="`basename \"$0\"`"
    
    cat >&2 <<EOF
Usage: $progname [options]
Creates upstream archives
Options:
  -u   Upload only upstream packages
  -d   Upload only debian packages
  -h   Print this brief help
EOF
}

opt_debian=1
opt_upstream=1

# Command line
while [ $# != 0 ]; do
    value="`echo x\"$1\" | sed -e 's/^x-.//'`"
    case "$1" in
        -h)  usage; exit 0           ;;
        -u)  opt_debian=0            ;;
        -d)  opt_upstream=0          ;;
         *)  usage; exit 0           ;;
    esac
    shift
done

VERSION=`sed -n -e "s/${DEBPKG} (\(.*\)-[0-9.]).*/\1/p" < debian/changelog  |head -1`
DEBVERSION=`sed -n -e "s/${DEBPKG} (\(.*\)).*/\1/p" < debian/changelog  |head -1`

pushd /usr/local/src/Packages/${DEBPKG} > /dev/null

if [ "${opt_upstream}" == "1" ]; then
  echo "Uploading upstream files to web site"
  UPSTREAM_DIR=ftp.med-info.com:/home/ftp/pub/${PKG}/.
  scp ${PKG}-${VERSION}.tar.gz ${UPSTREAM_DIR}
  scp ${PKG}-${VERSION}.zip ${UPSTREAM_DIR}
  ssh ftp.med-info.com "(cd /opt/apache/htdocs/${PKG}.med-info.com; make)" &
fi

echo "Uploading to Debian site"
DEBIAN_DIR=ftp.med-info.com:/home/ftp/pub/debian/UploadQueue

if [ "${opt_upstream}" == "1" ]; then
  echo "...Uploading original upstream archive"
  scp ${DEBPKG}_${VERSION}.orig.tar.gz ${DEBIAN_DIR}
fi
scp ${DEBPKG}_${DEBVERSION}.diff.gz ${DEBIAN_DIR}
scp ${DEBPKG}_${DEBVERSION}.dsc ${DEBIAN_DIR}
scp ${DEBPKG}_${DEBVERSION}_*.deb ${DEBIAN_DIR}
scp ${DEBPKG}-*_${DEBVERSION}_*.deb ${DEBIAN_DIR}
scp ${DEBPKG}_${DEBVERSION}_*.changes ${DEBIAN_DIR} # upload last

popd > /dev/null


