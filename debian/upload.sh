#!/bin/bash -e

dup clsql -Uftp.med-info.com -D/home/ftp/clsql \
    -C"(cd /opt/apache/htdocs/clsql.med-info.com; make install-doc)"

