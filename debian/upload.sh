#!/bin/bash -e

dup clsql -Uftp.med-info.com -D/home/ftp/clsql -su \
    -C"(cd /opt/apache/htdocs/clsql.med-info.com; make install)" $*
