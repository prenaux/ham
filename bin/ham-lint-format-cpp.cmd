@echo off
REM !! DO NOT add quotes around %~dp0 !!
set HAM_HOME=%~dp0\..
set PATH="%~dp0";"%~dp0\nt-x86";%HAM_HOME%\toolsets\repos\nt-x86\git\bin\;%PATH%

bash.exe "%~dp0\ham-lint-format-cpp" %*

REM We just pause at the end, this batch file is meant to be wired in a UI
REM shortcut for which we'll always want to see the output before closing.
pause
