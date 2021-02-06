@ECHO off
SET ROOT=%cd%
cd %~dp0
cd ..\..\..\..\..\..\
SET LIB=../../java-advanced-2020/lib
@ECHO on

javac -cp .;%LIB%/* ru/ifmo/rain/mozhevitin/bank/*.java
rmiregistry.exe

@ECHO off
cd %ROOT%
@ECHO on



