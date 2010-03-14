#!/bin/sh

CLOJURE_JAR=$HOME/Programming/btfmri/lib/clojure/clojure.jar
java -cp $CLOJURE_JAR:. clojure.main daves_i_know.clj
