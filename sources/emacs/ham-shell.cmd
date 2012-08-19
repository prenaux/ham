@echo off
set THISDIR=%~dp0
set PATH=%THISDIR%;%PATH%
bash --rcfile %THISDIR%\ham_shell_init.sh %2 %3 %4 %5 %6 %7 %8 %9
