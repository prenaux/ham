::Import("lang.ni")
::Import("sexp.ni")
::Import("fs.ni")
::Import("json.ni")

function main() {
  local path = ::sexp.utils.getAfter(::GetArgs(),"-f","")
  local dir = path.getdir();
  ::println("Load action file: " + path)

  local f = ::fs.fileOpenRead(path);
  local arr = []

  local os = ::lang.getHostOS()
  ::println("Convert for [" + os +"]")

  if (os == "nt")
  {
    local exe = "cl.exe"
    while (f.Tell() != f.size) {
      local line = f.ReadStringLine();
      if (line.contains(exe)) {
        local obj = {}
        local file = line.after("-Tp\"").before("\"")
        obj["directory"] <- dir
        obj["command"] <- line.after("  ")
        obj["file"] <- dir+file
        arr.append(obj)
      }
    }
  }
  else
  {
    while (f.Tell() != f.size) {
      local line = f.ReadStringLine();
      line = line.trimleft();
      local _args = line.split(" ");
      if (_args.len() > 0) {
        local obj = {}
        obj["directory"] <- dir
        // obj["arguments"] <- _args
        obj["command"] <- line
        local file = _args[_args.len() - 1].replace("\"","");
        if (!file.startswith("/")) {
          file = dir + file;
        }
        obj["file"] <- file;
        // ::println("line -> " + ::json.toString(obj))
        arr.append(obj)
      }
    }
  }
::json.toFile(arr,"compile_commands.json",true)
  ::println("Done, wrote compile_commands.json.")
}
