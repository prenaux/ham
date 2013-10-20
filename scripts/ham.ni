/**
 *  Copyright (C) 2007-2013 TalanSoft, Pierre Renaux
 */
::Import("lang.ni")
::Import("fs.ni")

@namespace "ham" {
  _osArch = null
  _hamPath = null
  _hamHome = null
  _bashPath = null
  _tempDir = null
  _tempFilesCollector = []
  _debugEchoAll = true

  function getHamPath() {
    if (_hamPath)
      return _hamPath;

    local hamHome = ::gRootFS.GetAbsolutePath(::gLang.property["ni.dirs.main"] + "../ham");

    local hamPath = "".setdir(hamHome).adddirback("bin").SetFile("ham");
    if (!::gRootFS.FileExists(hamPath,::eFileAttrFlags.AllFiles))
      throw "Can't find ham executable in: " + hamHome

    _hamHome = "".setdir(hamHome);
    _hamPath = hamPath;
    return hamPath;
  }

  function getHamHome() {
    if (_hamHome)
      return _hamHome;

    // will set ham home
    getHamPath();
    return _hamHome;
  }

  function getOSArch() {
    if (_osArch)
      return _osArch;

    _osArch = ::gLang.property["ni.loa.os"] + "-" + ::gLang.property["ni.loa.arch"];
    return _osArch;
  }

  function getBinDir() {
    return "bin/" + getOSArch();
  }

  function getBashPath() {
    if (_bashPath)
      return _bashPath

    local bashPath
    switch (::lang.getHostOS().tolower()) {
      case "nt": {
        bashPath = getHamHome().adddirback(getBinDir()).setfile("bash.exe")
        if (!::gRootFS.FileExists(bashPath,::eFileAttrFlags.AllFiles))
          throw "Can't find bash executable in: " + bashPath
        break;
      }
      default: {
        throw "Unknown platform to found bash's path !"
      }
    }

    _bashPath = bashPath
    return _bashPath;
  }

  function getTempDir() {
    if (_tempDir)
      return _tempDir;

    local tempDir

    tempDir = "".setdir(::gLang.property["ni.dirs.temp"])
    if (::fs.dirExists(tempDir)) {
      _tempDir = tempDir;
      return _tempDir;
    }

    tempDir = "".setdir(::gLang.env["TEMP"])
    if (::fs.dirExists(tempDir)) {
      _tempDir = tempDir;
      return _tempDir;
    }

    tempDir = "".setdir(::gLang.env["TMP"])
    if (::fs.dirExists(tempDir)) {
      _tempDir = tempDir;
      return _tempDir;
    }

    throw "Can't find the temp folder !"
  }

  function getNewTempFilePath(aExt,aName) {
    local path = getTempDir().setfile((aName || "")+::gLang.CreateGlobalUUID()).setext(aExt || "tmp")
    _tempFilesCollector.Add(path)
    ::println("... Added temp file:" path)
    return path;
  }

  function deleteTempFiles() {
    local filesToCollect = _tempFilesCollector
    _tempFilesCollector = []
    foreach (f in filesToCollect) {
      local r = ::gRootFS.FileDelete(f)
      ::println("... Deleted temp file:" f "("+(r ? "yes" : "didnt exist")+")")
    }
  }

  // Return {
  //          succeeded = true|false,
  //          exitCode = program exit code,
  //          stdout = if abKeepStdOut "output in stdout",
  //          stderr = if abKeepStdOut "output to stderr"
  //        }
  function runProcess(aCmd,abKeepStdOut,abEchoStdout) {
    local pm = ::gLang.process_manager
    local curProc = pm.current_process
    local proc = pm.SpawnProcess(aCmd,::eOSProcessSpawnFlags.StdFiles)
    if (!proc)
      throw "Couldn't spawn process from command line: " + aCmd

    local stdout = ""
    local stderr = ""

    do {
      local procStdOut = proc.file[1]
      local procStdErr = proc.file[2]
      local validCount = 0

      // drain stdout to stdout
      if (::lang.isValid(procStdOut)) {
        ++validCount
        local line = procStdOut.ReadStringLine()
        if (!line.?empty()) {
          if (abEchoStdout || _debugEchoAll) {
            if (_debugEchoAll) {
              curProc.file[1].WriteString("D/RUN-STDOUT: ")
            }
            curProc.file[1].WriteString(line)
            curProc.file[1].WriteString("\n")
          }
          if (abKeepStdOut) {
            stdout += line + "\n"
          }
        }
      }

      // drain stderr to stdout
      if (::lang.isValid(procStdErr)) {
        ++validCount
        local line = procStdErr.ReadStringLine()
        if (!line.?empty()) {
          if (abEchoStdout || _debugEchoAll) {
            if (_debugEchoAll) {
              curProc.file[1].WriteString("D/RUN-STDERR: ")
            }
            curProc.file[1].WriteString(line)
            curProc.file[1].WriteString("\n")
          }
          if (abKeepStdOut) {
            stderr += line + "\n"
          }
        }
      }

      if (validCount == 0)
        break

    } while(1)

    local procRet = proc.WaitForExitCode();
    return {
      succeeded = procRet.x
      exitCode = procRet.y
      stdout = stdout
      stderr = stderr
    }
  }

  function seqProcess(aCmd) {
    local r = {
      _cmd = aCmd

      function runCommand() {
        _count <- 0
        _pm <- ::gLang.process_manager
        _curProc <- _pm.current_process
        _proc <- _pm.SpawnProcess(_cmd,::eOSProcessSpawnFlags.StdFiles)
        if (!_proc)
          throw "Couldn't spawn process from command line: " + _cmd
        _procStdOut <- _proc.file[1]
        _procStdErr <- _proc.file[2]
      }

      function empty() {
        return _cmd.?empty()
      }

      function _nexti(itr) {
        if (itr == null) {
          // initialize _fileStart if not already done
          runCommand()
          // don't reset the counter here...
          itr = [_count,_count]
        }

        if (itr[1].?stopped) {
          return null;
        }

        local validCount = 0
        local r = {
          pid = _proc.?pid
          proc = _proc
          stopped = false
          succeeded = null
          exitCode = invalid
          stdoutLine = ""
          stderrLine = ""
        }

        local debugEchoAll = ::ham._debugEchoAll

        // drain stdout
        if (::lang.isValid(_procStdOut)) {
          ++validCount
          local line = _procStdOut.ReadStringLine()
          if (!line.?empty()) {
            if (debugEchoAll) {
              _curProc.file[1].WriteString("D/SEQ-STDOUT: " + line + "\n")
            }
            r.stdoutLine = line
          }
        }

        // drain stderr
        if (::lang.isValid(_procStdErr)) {
          ++validCount
          local line = _procStdErr.ReadStringLine()
          if (!line.?empty()) {
            if (debugEchoAll) {
              _curProc.file[1].WriteString("D/SEQ-STDERR: " + line + "\n")
            }
            r.stderrLine = line
          }
        }

        if (validCount == 0) {
          local ret = _proc.WaitForExitCode()
          r.stopped = true
          r.succeeded = ret.x
          r.exitCode = ret.y
        }

        itr[0] = _count++;
        itr[1] = r;
        return itr;
      }
    }

    r.__SetCanCallMetaMethod(true)
    return r
  }

  function runBash(aScript,abKeepStdOut,abEchoStdout) {
    local tmpFilePath = getNewTempFilePath("sh")
    ::fs.writeString(aScript,tmpFilePath)
    if (_debugEchoAll) {
      ::println(::format("I/Running from %s\n-----------------------\n%s\n-----------------------"
                         tmpFilePath, aScript));
    }
    return runProcess(getBashPath() + " " + tmpFilePath,abKeepStdOut,abEchoStdout)
  }

  function seqBash(aScript) {
    local tmpFilePath = getNewTempFilePath("sh")
    ::fs.writeString(aScript,tmpFilePath)
    if (_debugEchoAll) {
      ::println(::format("I/Running from %s\n-----------------------\n%s\n-----------------------"
                         tmpFilePath, aScript));
    }
    return seqProcess(getBashPath() + " " + tmpFilePath)
  }
}
