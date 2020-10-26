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
    local exe = "bin/cl.exe"
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
    local key = "  SRC_ABSPATH="
    local pwd = "`pwd`"
    local abs = "${SRC_ABSPATH}"

    while (f.Tell() != f.size) {
      local line = f.ReadStringLine();
      if (line.StartsWith(key)) {
        local obj = {}
        local abs_path = line.after(key)
        abs_path = abs_path.replace(pwd,dir)
        abs_path = abs_path.replace("\"","")
        obj["directory"] <- dir
        local command = f.ReadStringLine();
        command = command.replace(abs,abs_path)
        obj["command"] <- command
        obj["file"] <- abs_path
        arr.append(obj)
      }
    }
  }
  ::json.toFile(arr,"compile_commands.json",true)
  ::println("writed to compile_commands.json done.")
}