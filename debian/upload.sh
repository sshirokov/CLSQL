#!/bin/bash -e

dup clsql -Ufiles.b9.com -D/home/ftp/clsql -su \
    -C"(umask 022; cd /srv/www/html/clsql; make install; find . -type d |xargs chmod go+rx; find . -type f | xargs chmod go+r)" $*
