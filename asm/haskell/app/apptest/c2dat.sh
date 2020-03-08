#!/bin/sh
#
#
awk '/0x/ {
 gsub(/ /, "", $0);
 gsub(/0x/, "", $0)
 sub(/,$/, "", $0);
 split($0,  a, ",");
 for (d in a) { print a[d] } }' $*
