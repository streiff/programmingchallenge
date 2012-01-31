#!/bin/sh

ruby -pe 'gsub(/(.)/, "\\1\n")' < $1 |
    sort | 
    grep -v '^$' |
    tr -d '\n' | 
    sed 's#\(.\)\(\1*\)#\1\2\n#g' | 
    awk '{ a = substr($0, 1, 1); gsub(/./, "*", $0); printf "%s:%s\n", a, $0 }' 
