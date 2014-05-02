@echo off
set PATH=%~dp0;%~dp0\nt-x86
set HAM_HOME=%~dp0\..
"%~dp0\nt-x86\bash.exe" "%~dp0\emacs" %*
