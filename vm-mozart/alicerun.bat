@echo off

set "ALICE_LOAD=cache=%HOMEDRIVE%%HOMEPATH%/.alice/cache;pattern=x-oz:?{x}=x-oz:?{x};pattern=?{x}=?{x}.ozf;pattern=?{x}=?{x}"

if not "%ALICE_LOAD_PREFIX%"=="" set "ALICE_LOAD=%ALICE_LOAD_PREFIX%;%ALICE_LOAD%"
if not "%ALICE_LOAD_SUFFIX%"=="" set "ALICE_LOAD=%ALICE_LOAD%;%ALICE_LOAD_SUFFIX%"

if "%OZ_LOAD%"=="" set "OZ_LOAD=cache=%HOMEDRIVE%%HOMEPATH%/.oz/cache;cache=%OZHOME%/cache"

set "OZ_LOAD=%ALICE_LOAD%;pattern=x-alice:/?{x}=%ALICE_HOME%/?{x}.ozf;%OZ_LOAD%"

ozengine x-alice:/VMMain %1 %2 %3 %4 %5 %6 %7 %8 %9
