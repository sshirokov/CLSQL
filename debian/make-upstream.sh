#!/bin/bash -e

bups.sh -Sclsql -Dcl-sql -f"-name test.config" -d"-name .bin" || 
  (echo "Error building upstream" >& 2; exit 1)

exit 0

