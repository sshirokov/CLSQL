#!/bin/sh -e

dup clsql -Uftp.med-info.com -D/home/ftp/pub/clsql \
    -C"(cd /opt/apache/htdocs/clsql.med-info.com; make)"

