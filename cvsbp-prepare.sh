#!/bin/bash

set -e  # abort on error

echo "Cleaning checkout CVS directory"
rm -f upload.sh make-upstream.sh make-debian.sh
rm -f `find . -type f -name .cvsignore`
rm -f cvsbp-prepare.sh

