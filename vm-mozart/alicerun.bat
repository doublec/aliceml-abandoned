@echo off

if not defined OZHOME set OZHOME=%PROGRAMFILES%\Mozart
if not defined STOCKHOME set STOCKHOME=%PROGRAMFILES%\Alice

set ALICE_LOAD=pattern=x-oz:?{x}=x-oz:?{x};pattern=?{x}=?{x}.ozf;pattern=?{x}=?{x}

if defined ALICE_LOAD_PREFIX set ALICE_LOAD=%ALICE_LOAD_PREFIX%;%ALICE_LOAD%
if defined ALICE_LOAD_SUFFIX set ALICE_LOAD=%ALICE_LOAD%;%ALICE_LOAD_PREFIX%

if not defined OZ_LOAD set OZ_LOAD=cache=%HOMEDRIVE%%HOMEPATH%/.oz;cache=%OZHOME%/cache

set OZ_LOAD=%ALICE_LOAD%;pattern=x-alice:/?{x}=%STOCKHOME%/?{x}.ozf;%OZ_LOAD%

ozengine x-alice:/VMMain %1 %2 %3 %4 %5 %6 %7 %8 %9
