#!/bin/sh -e

debupload.sh -Sclsql -Dcl-sql -hftp.med-info.com -d/home/ftp/pub/clsql \
    -B"cl-sql cl-sql-base cl-sql-aodbc cl-sql-mysql cl-sql-postgresql cl-sql-postgresql-socket" \
    -c"(cd /opt/apache/htdocs/clsql.med-info.com; make)"

