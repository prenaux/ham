@echo off
REM Example usage:
REM   ham-toolset-dl-and-extract.cmd repos repos_nt-x86_v4
set PATH="%~dp0";"%~dp0\nt-x86"
REM !! DO NOT add quotes below !!
set HAM_HOME=%~dp0\..

setlocal EnableDelayedExpansion

if "%~1" == "" (
    echo Missing first argument. Syntax: %~nx0 TOOLSET_NAME ARCHIVE_NAME
    exit /b 1
)

if "%~2" == "" (
    echo Missing second argument. Syntax: %~nx0 TOOLSET_NAME ARCHIVE_NAME
    exit /b 1
)

set "CWD=%CD%"
set "DL_DIR=%HAM_HOME%\toolsets\_dl"
set "DIR=%HAM_HOME%\toolsets\%~1"
set "ARCH_URL=https://cdn2.talansoft.com/ftp/toolsets/%~2.7z"
REM set "ARCH_URL=http://localhost:8123/data/toolsets/%~2.7z"
set "DLFILENAME=_%~2.7z"

echo === Importing toolset '%~1' from %DIR%
mkdir "%DL_DIR%" 2>nul
mkdir "%DIR%" 2>nul
pushd "%DIR%" || (
    echo E/Can't cd to the toolset's directory '%DIR%'.
    exit /b 1
)

if exist "%DL_DIR%\%~2.7z" (
    echo I/Copying archive found at: %DL_DIR%\%~2.7z
    copy /Y "%DL_DIR%\%~2.7z" ".\%DLFILENAME%"
)

if errorlevel 1 (
    echo E/Can't cd to the toolset's directory '%DIR%'.
    exit /b 1
)

if exist "%DLFILENAME%" (
    echo I/Extracting %DLFILENAME%
    7z x -y "%DLFILENAME%"
    if errorlevel 1 (
        echo E/Extraction failed !
        del "%DLFILENAME%"
        popd
        exit /b 1
    )
    popd
) else (
    echo I/Trying download from %ARCH_URL%
    curl -L -o "%DLFILENAME%" "%ARCH_URL%"
    if errorlevel 1 (
        echo E/Download failed !
        popd
        exit /b 1
    )
    echo I/Extracting %DLFILENAME%
    7z x -y "%DLFILENAME%"
    if errorlevel 1 (
        echo E/Extraction failed !
        del "%DLFILENAME%"
        popd
        exit /b 1
    )
    popd
)

echo I/Done, downloaded and extracted toolset '%~1'.
