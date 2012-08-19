@echo off
set PATH=%~dp0;%~dp0\nt-x86
set HAM_HOME=%~dp0\..
%~dp0\nt-x86\bash.exe --rcfile %HAM_HOME%\bin\ham-bash-start.sh %2 %3 %4 %5 %6 %7 %8 %9
