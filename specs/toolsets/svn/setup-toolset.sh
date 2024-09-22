#!/bin/bash
toolset_import_once repos || return 1

# toolset
export HAM_TOOLSET=SVN
export HAM_TOOLSET_NAME=svn
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/svn"

# platform
case $HAM_OS in
  NT*)
    toolset_check_and_dl_ver svn nt-x86 v1_14_1 || return 1
    pathenv_add "${HAM_TOOLSET_DIR}/nt-x86/bin"
    # Mercifully msys git has git svn bundled in by default, so the repos toolset is enough.
    ;;
  OSX*)
    ham-brew-install svn "bin/svn" || return 1
    ;;
  LINUX*)
    if [ -z "$(which svn)" ]; then
      ham-apt-get-install subversion || return 1
    fi
    ;;
  *)
    echo "E/Toolset: Unsupported host OS"
    return 1
    ;;
esac

pathenv_add "${HAM_TOOLSET_DIR}"

# version check
if ! VER="--- svn --------------------------
$(svn --version | grep ", version")"; then
  echo "E/Can't get SVN version."
  return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
