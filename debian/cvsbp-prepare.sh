#!/bin/bash

set -e  # abort on error

echo "Cleaning checkout CVS directory"
rm -f `find . -type f -name .cvsignore`

