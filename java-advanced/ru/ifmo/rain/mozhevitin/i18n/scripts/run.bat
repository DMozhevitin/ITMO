@ECHO off
SET ROOT=%cd%
cd %~dp0
cd ..\..\..\..\..\..\
@ECHO on

java ru.ifmo.rain.mozhevitin.i18n.TextStatistics %*

@ECHO off
cd %ROOT%
@ECHO on