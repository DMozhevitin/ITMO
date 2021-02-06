@ECHO off
SET ROOT=%cd%
cd %~dp0
call compile.bat
cd ..\..\..\..\..\..\
SET LIB=../../java-advanced-2020/lib
@ECHO on

java -cp %LIB%\*;. org.junit.runner.JUnitCore  ru.ifmo.rain.mozhevitin.i18n.TextStatisticsTest

@ECHO off
cd %ROOT%
@ECHO on