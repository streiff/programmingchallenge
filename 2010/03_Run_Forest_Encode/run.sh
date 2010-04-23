#!/bin/sh

if [ -z $CLOJURE_JAR ]; then
  echo "CLOJURE_JAR environment variable must be set."
  exit
fi

if [ -z $CLOJURE_CONTRIB_JAR ]; then
  echo "CLOJURE_CONTRIB_JAR environment variable must be set."
  exit
fi

if [ $# -eq 3 -a \( \( "$1" = "encode" \) -o \( "$1" = "decode" \) \) ]; then
  java -cp $CLOJURE_JAR:$CLOJURE_CONTRIB_JAR:. clojure.main encoder.clj $1 $2 $3
else
  echo "Usage: $0 [encode|decode] [in.filename] [out.filename]"
fi
