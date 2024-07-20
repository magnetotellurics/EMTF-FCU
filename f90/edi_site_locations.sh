#!/bin/sh
if [ "$1" != "" ]; then
  dirname=$1
else
  dirname="."
fi
edi2xml.pl $dirname $dirname dry > tmp.txt
grep -v Rotate tmp.txt >site_locations.txt
rm tmp.txt
