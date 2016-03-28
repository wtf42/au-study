@echo off

set CUP_HOME=..\jflex-1.6.1\lib\java-cup-11a.jar
set JFLEX_HOME=..\jflex-1.6.1\lib\jflex-1.6.1.jar

java -classpath %CUP_HOME%;. java_cup.Main tokens.cup
java -classpath %JFLEX_HOME%;. jflex.Main llang.jflex
javac -classpath %CUP_HOME%;. LLexer.java TestLexer.java sym.java
REM java -classpath %CUP_HOME%;. TestLexer %1
