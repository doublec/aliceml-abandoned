@echo off

if not defined OZHOME set OZHOME=%PROGRAMFILES%\Mozart
if not defined STOCKHOME set STOCKHOME=%PROGRAMFILES%\Alice

set OZ_LOAD=pattern=?{x}=?{x}.ozf;pattern=x-alice:/?{x}=%STOCKHOME%/?{x}.ozf;pattern=x-alice:/?{x}=%STOCKHOME%/?{x};cache=%OZHOME%/cache

set ALICE_LOAD=pattern=x-oz:?{x}=x-oz:?{x};pattern=?{x}=?{x}.ozf;pattern=?{x}=?{x}

rem add ALICE_LOAD_PREFIX/ALICE_LOAD_SUFFIX #--**

set OZ_LOAD=%ALICE_LOAD%;%OZ_LOAD%

ozengine x-alice:/VMMain %1 %2 %3 %4 %5 %6 %7 %8 %9
