@echo off

set OZ_LOAD=pattern=?{x}=?{x}.ozf:pattern=x-alice:/?{x}=$STOCKHOME/?{x}.ozf:$OZ_LOAD

alicerun "%OZHOME%/bin/ozl.exe" %1 %2 %3 %4 %5 %6 %7 %8 %9
