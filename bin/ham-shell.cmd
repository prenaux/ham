@echo off
set PATH="%~dp0";"%~dp0\nt-x86"
REM !! DO NOT add quotes below !!
set HAM_HOME=%~dp0\..
bash --rcfile "%HAM_HOME%\bin\ham-bash-start.sh" -i %1 %2 %3 %4 %5 %6 %7 %8 %9
