#!/bin/bash

set -e  # abort on error

# Make documentation to put in debian package 
# This must match prebuilt upstream documentation

if [ -f doc/Makefile ]; then
  echo "Making upstream documentation for CVS checked-out package"
  cd doc
  make all
  cd ..
else
  echo "Unable to build documentation for CVS checked out package"
  exit 1
fi

# Clean checked out CVS directory
rm -f upload.sh make-upstream.sh make-debian.sh
rm -f `find . -type f -name .cvsignore`
rm -f cvsbp-prepare.sh
