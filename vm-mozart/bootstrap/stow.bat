@echo off

set OZ_LOAD=prefix=x-alice:/=D:/Program Files/stockhausen/;cache=D:/Program Files/Mozart/cache

set PATH=D:\Program Files\Mozart\bin\;%PATH%

ozengine x-alice:/stow.ozf %1 %2 %3 %4 %5 %6 %7 %8 %9
