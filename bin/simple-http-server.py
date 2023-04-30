# Python 3
import sys
import os
import socketserver
import argparse
import logging
import zipfile
from http.server import SimpleHTTPRequestHandler
from threading import Lock

# Set up the argument parser
parser = argparse.ArgumentParser(description='Run a static HTTP server.')
parser.add_argument('mode', choices=['regular', 'crossOriginIsolation'], help='The mode for the server.')
parser.add_argument('-d', '--dir', default='.', help='The directory to serve.')
parser.add_argument('-p', '--port', type=int, default=8123, help='The port to listen on.')
parser.add_argument('-l', '--log', default=None, help='The log file to store opened files.')
parser.add_argument('-z', '--zip', default=None, help='The zip file to store accessed files.')

# Parse the arguments
args = parser.parse_args()

mode = args.mode
print("I/Mode: " + mode)

dir = args.dir
if (not os.path.isdir(dir)):
    print("E/Directory doesnt exist: " + dir)
    printHelpAndExit()
print("I/Serving directory: " + os.path.abspath(dir))
os.chdir(dir)

log_file = args.log
if log_file:
    logging.basicConfig(level=logging.INFO, filename=log_file, filemode='a', format='%(asctime)s - %(message)s')
    print("I/Logging to: " + log_file)
else:
    logging.basicConfig(level=logging.ERROR)

accessed_files = set()
accessed_files_lock = Lock()

zip_file = args.zip
if zip_file:
    print("I/Zipping to: " + zip_file)

if zip_file and os.path.exists(zip_file):
    raise FileExistsError(f"The specified zip file '{zip_file}' already exists.")

encodings_map = {
    ".jsgz": "gzip",
    ".wasmgz": "gzip",
    ".symbolsgz": "gzip",
    ".memgz": "gzip",
    ".datagz": "gzip",
}

def handle_accessed_file(path):
  if (not os.path.isfile(path)):
    return

  with accessed_files_lock:
    rel_path = os.path.relpath(path, os.path.abspath(dir))
    if rel_path in accessed_files:
      return

    accessed_files.add(rel_path)

    # Log the requested file path
    if log_file:
      logging.info("Accessing:\n- rel: %s\n- abs: %s", rel_path, path)

    # Append the accessed file to the specified zip file if it hasn't been zipped yet
    if zip_file:
      with zipfile.ZipFile(zip_file, 'a') as zf:
        try:
          zf.write(path, arcname=rel_path)
        except Exception as e:
          logging.error("Error appending file to zip: %s", e)

class RequestHandler(SimpleHTTPRequestHandler):
    def end_headers(self):
        path = self.translate_path(self.path)
        extension = os.path.splitext(path)[1]

        handle_accessed_file(path)

        if extension in encodings_map:
            encoding = encodings_map[extension]
            self.send_header("Content-Encoding", encoding)

        self.send_header('Access-Control-Allow-Origin', '*')
        self.send_header('Access-Control-Allow-Methods', '*')
        self.send_header('Access-Control-Allow-Headers', '*')
        if mode == "crossOriginIsolation":
            self.send_header('Cross-Origin-Opener-Policy', 'same-origin')
            self.send_header('Cross-Origin-Embedder-Policy', 'require-corp')

        SimpleHTTPRequestHandler.end_headers(self)

# Python 3.7.5 adds in the WebAssembly Media Type. If this is an older
# version, add in the Media Type.
if sys.version_info < (3, 7, 5):
    RequestHandler.extensions_map['.wasm'] = 'application/wasm'

RequestHandler.extensions_map['.jsgz'] = 'application/x-javascript'
RequestHandler.extensions_map['.wasmgz'] = 'application/wasm'
RequestHandler.extensions_map['.symbolsgz'] = 'text/plain'
RequestHandler.extensions_map['.memgz'] = 'application/octet-stream'
# NOTE: .data is NOT compressed
RequestHandler.extensions_map['.data'] = 'application/octet-stream'
RequestHandler.extensions_map['.datagz'] = 'application/octet-stream'

if __name__ == '__main__':
    PORT = args.port
    Server = socketserver.ThreadingTCPServer
    Server.allow_reuse_address=True

    with Server(("", PORT), RequestHandler) as httpd:
        print("I/Serving at: http://localhost:{}".format(PORT))
        print("I/Press Ctrl+C to stop.".format(PORT))
        httpd.serve_forever()
