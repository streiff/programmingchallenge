#!/bin/sh

DIR=`dirname $PWD/$0`
CLOJURE_JAR="$DIR/clojure-1.7.0/clojure-1.7.0.jar"

java -cp $CLOJURE_JAR clojure.main $1
