@ECHO off
SET ROOT=%cd%
cd %~dp0
cd ..\..\..\..\..\..\
@ECHO on

java ru.ifmo.rain.mozhevitin.bank.Client %*

@ECHO off
cd %ROOT%
@ECHO on