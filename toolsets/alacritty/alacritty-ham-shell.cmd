@echo off
setlocal EnableDelayedExpansion

REM !! DO NOT add quotes around %~dp0 !!
set HAM_HOME=%~dp0\..\..
set PATH="%~dp0";"%~dp0\nt-x86";%HAM_HOME%\toolsets\repos\nt-x86\git\bin\;%PATH%

call "%HAM_HOME%\bin\ham-install-for-windows.cmd"
if errorlevel 1 (
    echo E/ham-install-for-windows.cmd failed.
    exit /b 1
)

del "%USERPROFILE%\_alacritty_last_cwd.txt"

set "HAM_HOME_FWD=%HAM_HOME:\=/%"

start "" /b /min "C:\Utils\Alacritty.exe" --config-file "%HAM_HOME_FWD%/toolsets/alacritty/alacritty-ham-shell.toml" ^
    -o "shell={program=\"K:/Work/ham/toolsets/repos/nt-x86/git/bin/bash.exe\",args=[\"--rcfile\",\"%HAM_HOME_FWD%/toolsets/alacritty/alacritty-ham-shell-start.sh\",\"-i\"]}"

exit
