# Test Thrift

Thrift Hello world Test

# References

- https://thrift.apache.org/
- https://thrift.apache.org/tutorial/
- https://github.com/diwakergupta/thrift-missing-guide
- http://diwakergupta.github.io/thrift-missing-guide/thrift.pdf

# Example compile + run

1. Setup the ham environment:
```
$ . hat
```

2. Run the server first and leave it running in a terminal:
```
$ ham Run_ham-test-thrift-server
...
=== Running: /Volumes/MBPWork/PR/pierre/bin/osx-arm64/ham-test-thrift-server_ra
/Volumes/MBPWork/PR/pierre/bin/osx-arm64
Starting the server...
```

2b. This will print after one client connected:
```
Incoming connection
	SocketInfo: <Host: ::1 Port: 61885>
	PeerHost: localhost
	PeerAddress: ::1
	PeerPort: 61885
ping()
add(1, 1)
calculate(1, Work(num1=1, num2=0, op=DIVIDE, comment=<null>))
calculate(1, Work(num1=15, num2=10, op=SUBTRACT, comment=<null>))
getStruct(1)
```

3. Run the C++ client:
```
$ ham Run_ham-test-thrift-client
...
=== Running: /Volumes/MBPWork/PR/pierre/bin/osx-arm64/ham-test-thrift-client_ra
/Volumes/MBPWork/PR/pierre/bin/osx-arm64
ping()
1 + 1 = 2
InvalidOperation: Cannot divide by 0
15 - 10 = 5
Received log: SharedStruct(key=1, value=5)
# runInDir_ Run_ham-test-thrift-client
...updated 1 target(s)...
...targets Run_ham-test-thrift-client...
...ham ran in 0s...
```

4. Run the HTML client:
```
# In yet another terminal...
./_serve_local.sh
# Open the local URL
open http://localhost:8123/src_html/JsClient.html
```

5. You can use CURL with the C++ server compiled with `USE_JSON_PROTOCOL`:
```
$ curl -X POST -H "Content-Type: application/x-thrift" -d '[1,"calculate",1,0,{"1":{"i32":1},"2":{"rec":{"1":{"i32":20},"2":{"i32":5},"3":{"i32":1}}}}]' http://localhost:40990/service

# Prints
[1,"calculate",2,0,{"0":{"i32":25}}]
```

6. Run the PHP client:
```
# Make sure the C++ server is running
./_run_php.sh
```

# Manage multiple processes with pm2

The short version:
```
# Start all servers which are no managed by pm2
./_pm2_start.sh

# Run something that use them...
./_run_php.sh

# Stop all servers
./_pm2_stop.sh
```

Various PM2 commands:
```
# Check the pm2 version number
ham-pm2 -v

# Start the static file hosting to test the HTML/JS samples
ham-pm2 start ./_serve_static_files.sh --name local-ham-test-thrift-static-files

# Build & start the C++ Server
ham-pm2 start ./_serve_cpp_server.sh --name local-ham-test-thrift-cpp-server

# List all running processes
ham-pm2 list

# See the list of running processes in a TUI
ham-pm2 monit

# Restart (and rebuild) the C++ server
ham-pm2 restart local-ham-test-thrift-cpp-server

# Stop the C++ server
ham-pm2 stop local-ham-test-thrift-cpp-server

# Look at the last logs of the specified process
# Remark: This tails the log so new logs will appear as long as the
# command is running
ham-pm2 logs local-ham-test-thrift-cpp-server

# Stop all the online processes
ham-pm2 stop all
```

References:
- https://pm2.keymetrics.io/docs/usage/quick-start/
- https://pm2.keymetrics.io/
- Multiple PM2 on the same server, cf https://pm2.keymetrics.io/docs/usage/specifics/
