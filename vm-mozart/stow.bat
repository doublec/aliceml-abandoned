@echo off

set OZ_LOAD=pattern=?{x}=?{x}.ozf;pattern=x-alice:/?{x}=D:/Program Files/stockhausen/?{x}.ozf;cache=D:/Program Files/Mozart/cache

set PATH=D:\Program Files\Mozart\bin\;%PATH%

ozengine x-alice:/StowMain %1 %2 %3 %4 %5 %6 %7 %8 %9
