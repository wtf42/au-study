@echo off

set CUP_HOME=..\jflex-1.6.1\lib\java-cup-11a.jar
set JFLEX_HOME=..\jflex-1.6.1\lib\jflex-1.6.1.jar

java -classpath %CUP_HOME%;. TestLexer %1
