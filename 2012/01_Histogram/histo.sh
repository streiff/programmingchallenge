#!/bin/sh

ruby -pe 'gsub(/(.)/, "\\1\n")' < $1 |
    sort | 
    tr -d '\n' |
    awk '{s=l=""; for(i=1;i<=length;i++){c=substr($1, i, 1); if(l!=c){s=s "\n" c ": "} s=s "*";l=substr($1,i,1)} print substr(s,2)}'
