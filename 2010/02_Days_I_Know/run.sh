#!/bin/sh

if [ -z $CLOJURE_JAR ]; then
  echo "CLOJURE_JAR environment variable must be set."
  exit
fi

java -cp $CLOJURE_JAR:. clojure.main daves_i_know.clj
