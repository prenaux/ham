@echo off
setlocal EnableDelayedExpansion

REM !! DO NOT add quotes around %~dp0 !!
set HAM_HOME=%~dp0\..
set PATH="%~dp0";"%~dp0\nt-x86";%HAM_HOME%\toolsets\repos\nt-x86\git\bin\;%PATH%

REM Check if bash.exe exists, and if not, call ham-toolset-dl-and-extract.cmd
if not exist "%HAM_HOME%\toolsets\repos\nt-x86\git\bin\bash.exe" (
    echo I/bash.exe not found downloading the repos toolset...
    call "%HAM_HOME%\bin\ham-toolset-dl-and-extract.cmd" repos repos_nt-x86_v4
    if errorlevel 1 (
        echo E/Failed to download and extract the toolset.
        echo Press any key to exit...
        pause >nul
        exit /b 1
    )
    echo I/repos toolset downloaded
)

REM Check again if bash.exe exists after running ham-toolset-dl-and-extract.cmd
if not exist "%HAM_HOME%\toolsets\repos\nt-x86\git\bin\bash.exe" (
    echo E/bash.exe not found. Cannot proceed.
    echo Press any key to exit...
    pause >nul
    exit /b 1
)
