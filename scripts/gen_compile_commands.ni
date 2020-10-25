::Import("lang.ni")
::Import("sexp.ni")
::Import("fs.ni")
::Import("json.ni")

function main() {
  local path = ::sexp.utils.getAfter(::GetArgs(),"-f","")
  local dir = path.getdir();
  ::println("Load action file: " + path)

  local f = ::fs.fileOpenRead(path);
  local key = "  SRC_ABSPATH="
  local pwd = "`pwd`"
  local abs = "${SRC_ABSPATH}"

  local arr = []
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

  ::json.toFile(arr,"compile_commands.json",true)
  ::println("writed to compile_commands.json done.")
}