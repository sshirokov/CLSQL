#!/bin/bash

set -e  # abort on error

# Clean checked out CVS directory
rm -f debian/upload.sh debian/make-upstream.sh debian/make-debian.sh
rm -f `find . -type f -name .cvsignore`
rm -f stamp-h.in build-stamp configure-stamp
rm -f debian/cvsbp-prepare.sh
