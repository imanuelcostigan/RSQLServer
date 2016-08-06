#!/bin/sh

rm inst/java/MSSQLRequestPull.jar
# Using Java 1.3 as this is what jTDS is built with
javac -d inst/java -source 1.3 -target 1.3 java/*.java || exit 1
(cd inst/java; jar fvc MSSQLRequestPull.jar com; rm -rf com)
