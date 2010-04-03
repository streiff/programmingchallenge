#!/bin/sh

if [ -z $CLOJURE_JAR ]; then
  echo "CLOJURE_JAR environment variable must be set."
  exit
fi

if [ -z $CLOJURE_CONTRIB_JAR ]; then
  echo "CLOJURE_CONTRIB_JAR environment variable must be set."
  exit
fi

if [ $# -ne 2 ]; then
  echo "Usage: $0 [encode|decode] [filename]"
elif [ $1 = "encode" ]; then
  java -cp $CLOJURE_JAR:$CLOJURE_CONTRIB_JAR:. clojure.main encoder.clj $2
elif [ $1 = "decode" ]; then
  java -cp $CLOJURE_JAR:$CLOJURE_CONTRIB_JAR:. clojure.main decoder.clj $2
else
  echo "Usage: $0 [encode|decode] [filename]"
fi
