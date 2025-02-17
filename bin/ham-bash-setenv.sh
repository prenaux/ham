#!/bin/bash

# This works around tools that manually reset the PATH environment variable.
if test -z "$PATH"; then
  if test -z "$PATH_BACKUP"; then
    echo "E/No PATH backup to restore"
  else
    export PATH=$PATH_BACKUP
  fi
fi

# Import the base library
. "$HAM_HOME/bin/ham-bash-lib.sh"

if [ "${HAM_ENV_SETUP}" != 1 ]; then
  ########################################################################
  ##  PATH
  ########################################################################
  export PATH=$BASH_START_PATH
  case $HAM_OS in
    NT*)
      pathenv_remove_add "$(unxpath "$WINDIR")/System32" after
      pathenv_add "$HAM_HOME/bin/nt-x86"
      pathenv_add "$HAM_HOME/toolsets/repos/nt-x86/git/bin"
      pathenv_add "$HAM_HOME/toolsets/repos/nt-x86/git/mingw64/bin"
      pathenv_add "$HAM_HOME/toolsets/repos/nt-x86/git/usr/bin"
      ;;
    OSX)
      pathenv_remove_add /opt/homebrew/bin after
      pathenv_remove_add /opt/homebrew/sbin after
      pathenv_remove_add /usr/local/bin after
      pathenv_remove_add /usr/bin after
      pathenv_remove_add /usr/sbin after
      pathenv_remove_add /bin after
      pathenv_remove_add /sbin after
      pathenv_remove_add /Library/Apple/usr/bin after
      pathenv_add "$("$HAM_HOME/bin/ham-brew-installdir" prefix)/bin"
      pathenv_add "$HAM_HOME/bin/osx"
      ;;
    LINUX)
      pathenv_remove_add /usr/local/sbin after
      pathenv_remove_add /usr/local/bin after
      pathenv_remove_add /usr/sbin after
      pathenv_remove_add /usr/bin after
      pathenv_remove_add /sbin after
      pathenv_remove_add /bin after
      pathenv_remove_add /snap/bin after
      pathenv_add "$("$HAM_HOME/bin/ham-brew-installdir" prefix)/bin"
      pathenv_add "$HAM_HOME/bin/linux"
      ;;
    *)
      echo "W/ham-bash-setenv.sh: Unknown HAM_OS: $HAM_OS"
      ;;
  esac

  pathenv_add "$HAM_HOME/bin/$HAM_BIN_LOA"
  # This must be called after bin/HAM_BIN_LOA so that they
  # get before it in the PATH list.
  pathenv_add "$HAM_HOME/bin"
  pathenv_add "$WORK/niLang/bin"
  export PATH_BACKUP=$PATH

  # Sanitize HAM_HOME's path
  HAM_HOME=$(abspath "$HAM_HOME")
  export HAM_HOME

  # Detect the package manager
  HAM_OS_PACKAGE_MANAGER=$(ham_os_package_detect_default_manager)
  export HAM_OS_PACKAGE_MANAGER
else
  true
fi

export HAM_ENV_SETUP=1
