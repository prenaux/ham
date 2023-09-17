@echo off
REM !! DO NOT add quotes around %~dp0 !!
set HAM_HOME=%~dp0\..
set PATH="%~dp0";"%~dp0\nt-x86";%HAM_HOME%\toolsets\repos\nt-x86\git\bin\;%PATH%

call "%HAM_HOME%\bin\ham-install-for-windows.cmd"
if errorlevel 1 (
    echo E/ham-install-for-windows.cmd failed.
    exit /b 1
)

bash.exe "%~dp0\ham-fix" %*

