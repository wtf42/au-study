#!/bin/bash

java -classpath .:jflex-1.6.1/lib/java-cup-11a.jar java_cup.Main < tokens.cup
java -classpath .:jflex-1.6.1/lib/jflex-1.6.1.jar jflex.Main llang.jflex
javac -classpath .:jflex-1.6.1/lib/java-cup-11a.jar LLexer.java TestLexer.java sym.java
java -classpath .:jflex-1.6.1/lib/java-cup-11a.jar TestLexer $*
