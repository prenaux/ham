# Python 3
import argparse
import logging
import os
import socketserver
import subprocess
import sys
import time
import zipfile
from http.server import SimpleHTTPRequestHandler
from threading import Lock
from urllib.parse import parse_qs

# Set up the argument parser
parser = argparse.ArgumentParser(description='Run a static HTTP server.')
parser.add_argument('mode', choices=['regular', 'crossOriginIsolation'], help='The mode for the server.')
parser.add_argument('-d', '--dir', default='.', help='The directory to serve.')
parser.add_argument('-p', '--port', type=int, default=8123, help='The port to listen on.')
parser.add_argument('-l', '--log', default=None, help='The log file to store opened files.')
parser.add_argument('-z', '--zip', default=None, help='The zip file to store accessed files.')
parser.add_argument('-x', '--hamx', action='store_true', help='When a hamx query param is specified run an hamx before loading the page.')

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

class CommandExecutionError(Exception):
    def __init__(self, message):
        super().__init__(message)

class RequestHandler(SimpleHTTPRequestHandler):
    def do_GET_hamx(self, command):
        try:
            # Send the hamx command running message
            self.send_response(200)
            self.send_header('Content-type', 'text/html')
            self.end_headers()
            script_code = """<script>
function showHamCommandOutput(aState) {
var e = document.getElementById("ham-command-output");
var d = e.style.display;
if (aState == 'toggle') { e.style.display = (e.style.display == 'none') ? 'block' : 'none'; }
else if (aState) { e.style.display = 'block'; }
else { e.style.display = 'none'; }
}
</script>"""
            css_reset = "<style>*{margin: 0;padding: 0;list-style: none;}body{background:#222;overflow:visible !important;}</style>"
            message_style = 'color: #eee; background-color: #222; font-family: monospace; font-size: 0.7rem;'
            self.wfile.write(f'{script_code}{css_reset}<div id="ham-command" style="{message_style}"><span onclick="showHamCommandOutput(\'toggle\');">ham command: {command}</span>'.encode())

            # Run the ham command
            self.wfile.write(f'<pre id="ham-command-output" style="font-size: 1rem; word-break: break-word; white-space: pre-wrap;">'.encode())
            start_time = time.time()
            for output_line in self.run_ham_command(command):
                self.wfile.write(f'{time.time() - start_time:.3f}s: {output_line}'.encode())
            self.wfile.write(f'</pre>'.encode())
            self.wfile.write(f' [Ran in {time.time() - start_time:.3f}s]'.encode())
            self.wfile.write(f'<script>showHamCommandOutput(false);</script>'.encode())
            self.wfile.write(f'</div>'.encode())

            # Send the file content
            file_path = self.translate_path(self.path)
            if not os.path.isfile(file_path):
                raise FileNotFoundError(f"The file '{file_path}' does not exist.")

            with open(file_path, 'r') as file:
                file_content = file.read()
                self.wfile.write(file_content.encode())

        except CommandExecutionError as e:
            # Send a 500 response with the exception message, stdout, and stderr
            self.wfile.write(f'<script>showHamCommandOutput(true);</script>'.encode())
            self.send_response(500)
            self.send_header('Content-type', 'text/html')
            self.end_headers()
            error_message = "<pre>"
            error_message += f"Error: {e}\n\n"
            error_message += "</pre>"
            self.wfile.write(error_message.encode())
            # Log the exception
            self.log_error(f"Exception during command execution: {e}")
            return

        except FileNotFoundError as e:
            # Send a 404 response if the file doesn't exist
            self.send_response(404)
            self.end_headers()
            error_message = f"<pre>File not found: {e}</pre>"
            self.wfile.write(error_message.encode())
            return

    def do_GET(self):
        query_params = parse_qs(self.path.split('?', 1)[-1])
        if args.hamx and ('hamx' in query_params):
            command = query_params['hamx'][0]
            self.do_GET_hamx(command)
        else:
            super().do_GET()

    def run_ham_command(self, command):
        try:
            self.log_message(f"I/run_hamx_command: '{command}'")

            command_args = command.split(",")
            if len(command_args) < 2:
                raise CommandExecutionError(f"run_hamx_command: '{command}': Invalid command format")

            # We run from the WORK folder
            working_directory = os.environ.get('WORK')
            if working_directory is None:
                raise CommandExecutionError(f"run_hamx_command: '{command}': WORK environment variable is not set")

            transformed_command = f"hamx {' '.join(command_args)}"
            self.log_message(f"I/running: '{transformed_command}'")

            # Setup an environment that should disable terminal colors...
            clean_env = os.environ.copy()
            for env_var in ['TERM', 'COLOR', 'LS_COLORS', 'CLICOLOR']:
                if env_var in clean_env:
                    del clean_env[env_var]
            clean_env['NO_COLOR'] = '1'

            # Run the process
            process = subprocess.Popen(
                transformed_command,
                stdout=subprocess.PIPE,
                # stderr=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                shell=True,
                text=True,
                cwd=working_directory,
                env=clean_env
            )

            for line in process.stdout:
                o = line.strip();
                o = f"stdout: {o}\n"
                self.log_message(o)
                yield o

            # for line in process.stderr:
            #     o = line.strip();
            #     o = f"stderr: {o}]\n"
            #     self.log_message(o)
            #     yield o

            process.wait()

            # Check the return code to see if the command failed
            if process.returncode != 0:
                raise CommandExecutionError(f"Command execution failed: {command_args}")

            # Log a completion message
            self.log_message("Command execution complete.")

        except CommandExecutionError as e:
            # Just forward this
            raise e
        except Exception as e:
            # Transform other exceptions into CommandExecutionError
            raise CommandExecutionError(f"Exception during command execution: {command_args}: {e}")

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
