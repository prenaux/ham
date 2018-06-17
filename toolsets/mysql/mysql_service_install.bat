@echo off
set HAM_HOME="%~dp0%..\..\..\ham"
set WORK="%HAM_HOME%\..\"
set MYSQL_DB_DIR="%WORK%/Server/mysql"
set /p SERVICE_NAME="Enter the service name: "
echo # Installing MySQL service '%SERVICE_NAME%'
"%HAM_HOME%\toolsets\mysql\nt-x86\bin\mysqld.exe" --install %SERVICE_NAME% --defaults-file="%MYSQL_DB_DIR%/my.cnf"
echo # Starting MySQL service...
net start ts_mysql
pause
