@echo off

set OZHOME=C:\Program Files\Mozart
set STOCKHOME=Y:\.root\opt\stockhausen-operette2

set PATH=%STOCKHOME%\bin;%PATH%
set OZ_LOAD=pattern=?{x}=?{x}.ozf;pattern=x-alice:/?{x}=%STOCKHOME%/?{x}.ozf;pattern=x-alice:/?{x}=%STOCKHOME%/?{x};cache=%OZHOME%/cache

stow "%OZHOME%/bin/ozl.exe" %1 %2 %3 %4 %5 %6 %7 %8 %9
