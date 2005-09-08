#!/bin/bash -e

dup clsql -Uftp.med-info.com -D/home/ftp/clsql -su \
    -C"(umask 022; cd /opt/apache/htdocs/clsql; make install; find . -type d |xargs chmod go+rx; find . -type f | xargs chmod go+r)" $*
