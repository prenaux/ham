@echo off
set HAM_HOME="%~dp0%..\..\..\ham"
set WORK="%HAM_HOME%\..\"
set MYSQL_DB_DIR="%WORK%/Server/mysql"
set /p SERVICE_NAME="Enter the service name: "
echo # Stopping MySQL service...
net stop ts_mysql
echo # Installing MySQL service '%SERVICE_NAME%'
"%HAM_HOME%\toolsets\mysql\nt-x86\bin\mysqld.exe" --remove %SERVICE_NAME%
pause
