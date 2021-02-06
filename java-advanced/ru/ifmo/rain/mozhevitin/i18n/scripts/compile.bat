SET ROOT=%cd%
cd %~dp0
cd ..\..\..\..\..\..\
SET LIB=../../java-advanced-2020/lib

javac -encoding UTF8 -cp .;%LIB%/* ru/ifmo/rain/mozhevitin/i18n/bundle/*.java
javac -encoding UTF8 -cp .;%LIB%/* ru/ifmo/rain/mozhevitin/i18n/*.java

@ECHO off
cd %ROOT%
@ECHO on