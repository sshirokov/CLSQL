#!/bin/sh -e

debupload.sh -Sclsql -Dcl-sql -Uftp.med-info.com -R/home/ftp/pub/clsql \
    -C"(cd /opt/apache/htdocs/clsql.med-info.com; make)"

