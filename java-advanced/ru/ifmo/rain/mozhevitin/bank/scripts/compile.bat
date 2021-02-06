SET ROOT=%cd%
cd %~dp0
cd ..\..\..\..\..\..\
SET LIB=../../java-advanced-2020/lib

javac -cp .;%LIB%/* ru/ifmo/rain/mozhevitin/bank/*.java

@ECHO off
cd %ROOT%
@ECHO on