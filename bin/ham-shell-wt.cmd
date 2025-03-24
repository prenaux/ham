@echo off
setlocal EnableDelayedExpansion

REM !! DO NOT add quotes around %~dp0 !!
set HAM_HOME=%~dp0\..
set PATH="%~dp0";"%~dp0\nt-x86";%HAM_HOME%\toolsets\repos\nt-x86\git\bin\;%PATH%

call "%HAM_HOME%\bin\ham-install-for-windows.cmd"
if errorlevel 1 (
    echo E/ham-install-for-windows.cmd failed.
    exit /b 1
)

REM Check if Windows Terminal exists at the specified path
set WT_PATH=c:/Utils/terminal/wt.exe
if exist "%WT_PATH%" (
    REM Start bash in Windows Terminal with the current directory and custom title
    "%WT_PATH%" -d "%CD%" --title "ham-shell" bash.exe --rcfile "%HAM_HOME%\bin\ham-bash-start.sh" -i %*
) else (
    REM Fallback to regular bash if Windows Terminal is not found
    bash.exe --rcfile "%HAM_HOME%\bin\ham-bash-start.sh" -i %*
)
