#!/bin/sh

set -e # abort on error

PKG=clsql
DEBPKG=cl-sql


WORK_DIR=/usr/local/src/Work/${PKG}
PKG_DIR=/usr/local/src/Packages/${DEBPKG}

UPSTREAM_DIR=ftp.med-info.com:/home/ftp/pub/${PKG}/.
FTP_DEBDIR=ftp.med-info.com:/home/ftp/pub/debian/UploadQueue


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

pushd ${PKG_DIR} > /dev/null

if [ "${opt_upstream}" == "1" ]; then
  echo "Uploading upstream files to web site"
  scp ${PKG}-${VERSION}.tar.gz ${UPSTREAM_DIR}
  scp ${PKG}-${VERSION}.zip ${UPSTREAM_DIR}
  ssh ftp.med-info.com "(cd /opt/apache/htdocs/${PKG}.med-info.com; make)" &
fi

echo "Uploading to Debian site"

if [ "${opt_upstream}" == "1" ]; then
  echo "...Uploading original upstream archive"
  scp ${DEBPKG}_${VERSION}.orig.tar.gz ${FTP_DEBDIR}
fi
scp ${DEBPKG}_${DEBVERSION}.diff.gz ${FTP_DEBDIR}
scp ${DEBPKG}_${DEBVERSION}.dsc ${FTP_DEBDIR}
scp ${DEBPKG}_${DEBVERSION}_*.deb ${FTP_DEBDIR}
scp ${DEBPKG}-*_${DEBVERSION}_*.deb ${FTP_DEBDIR}
scp ${DEBPKG}_${DEBVERSION}_*.changes ${FTP_DEBDIR} # upload last

popd > /dev/null


