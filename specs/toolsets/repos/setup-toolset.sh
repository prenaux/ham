#!/bin/bash

# toolset
export HAM_TOOLSET=REPOS
export HAM_TOOLSET_NAME=repos
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/repos"

# platform
case $HAM_OS in
  NT*)
    toolset_check_and_dl_ver repos nt-x86 v4 || return 1
    export REPOS_DIR="${HAM_TOOLSET_DIR}/nt-x86"
    export OPENSSL_CONF="${REPOS_DIR}/git/ssl/openssl.cnf"

    pathenv_add "${HAM_TOOLSET_DIR}"
    pathenv_add "${REPOS_DIR}/bin" after
    pathenv_add "${REPOS_DIR}/git/bin" after
    pathenv_add "${REPOS_DIR}/git/usr/bin" after
    pathenv_add "${REPOS_DIR}/hg" after

    # Try our best to disable that insanity, hands off my files keep them
    # as they are in the repo ffs.
    git config --global core.autocrlf false
    ;;
  OSX*)
    pathenv_add "${HAM_TOOLSET_DIR}"
    ;;
  LINUX*)
    pathenv_add "${HAM_TOOLSET_DIR}"
    ;;
  *)
    echo "E/Toolset: Unsupported host OS"
    return 1
    ;;
esac

# version check
VER="--- repos ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  if ! VER="$VER
--- git ---
$(git --version)"; then
    echo "E/Can't get Git version."
    return 1
  fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"

HG_PATH=$(where_inpath hg || true)
if [ -e "$HG_PATH" ]; then
  VER="--- mercurial ---"
  if [ "$HAM_NO_VER_CHECK" != "1" ]; then
    if ! VER="$VER
$(hg --version | grep version)"; then
      echo "E/Can't get Mercurial version."
      return 1
    fi
  fi
else
  VER="--- mercurial ---"
  if [ "$HAM_NO_VER_CHECK" != "1" ]; then
    VER="$VER
W/Mercurial is not installed or not accessible from the PATH !"
  fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
