# Python 3
import sys
import os
import socketserver
from http.server import SimpleHTTPRequestHandler

# Needed for SharedArrayBuffer support
crossOriginIsolation = False

def printHelpAndExit():
    print("Syntax:")
    print("  simple-http-server mode dir")
    print("Modes:")
    print("  - regular: Simple standard HTTP server.")
    print("  - wasm: HTTP server with cross origin isolation.\n" +
          "          It can only load files under the same domain.")
    exit(1)

if len(sys.argv) > 1:
    mode = sys.argv[1]
    if (mode == "regular"):
        crossOriginIsolation = False
    elif (mode == "wasm"):
        crossOriginIsolation = True
    else:
        print("E/Unknown server mode specified: " + mode)
        printHelpAndExit()
    print("I/Mode: " + mode)

    if len(sys.argv) > 2:
        dir = sys.argv[2]
    else:
        dir = "."
    if (not os.path.isdir(dir)):
        print("E/Directory doesnt exist: " + dir)
        printHelpAndExit()
    print("I/Serving directory: " + os.path.abspath(dir))
    os.chdir(dir)

else:
    print("E/Missing parameter.")
    printHelpAndExit()

class RequestHandler(SimpleHTTPRequestHandler):
    def end_headers(self):
        self.send_header('Access-Control-Allow-Origin', '*')
        self.send_header('Access-Control-Allow-Methods', '*')
        self.send_header('Access-Control-Allow-Headers', '*')
        if crossOriginIsolation:
            self.send_header('Cross-Origin-Opener-Policy', 'same-origin')
            self.send_header('Cross-Origin-Embedder-Policy', 'require-corp')
        SimpleHTTPRequestHandler.end_headers(self)

# Python 3.7.5 adds in the WebAssembly Media Type. If this is an older
# version, add in the Media Type.
if sys.version_info < (3, 7, 5):
    RequestHandler.extensions_map['.wasm'] = 'application/wasm'

if __name__ == '__main__':
    PORT = 8123
    with socketserver.TCPServer(("", PORT), RequestHandler) as httpd:
        print("Listening on port {}. Press Ctrl+C to stop.".format(PORT))
        httpd.serve_forever()
