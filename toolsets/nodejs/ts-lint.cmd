@echo off
REM !! DO NOT add quotes below !!
set HAM_HOME=%~dp0%..\..
set PATH="%~dp0";"%HAM_HOME%\bin\nt-x86\";"%HAM_HOME%\toolsets\nodejs\nt-x86\"
"%HAM_HOME%\toolsets\nodejs\nt-x86\tsc" --module commonjs --noEmit %*
