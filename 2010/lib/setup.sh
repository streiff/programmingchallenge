#!/bin/sh

DIR=`dirname $PWD/$0`

export CLOJURE_JAR="$DIR/clojure.jar"
export CLOJURE_CONTRIB_JAR="$DIR/clojure-contrib.jar"

echo "Setting CLOJURE_JAR to $CLOJURE_JAR"
echo "Setting CLOJURE_CONTRIB_JAR to $CLOJURE_CONTRIB_JAR"
