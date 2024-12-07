#!/bin/bash
case $HAM_OS in
  OSX*)
    # On macOS we just assume xcode and the cmd line tools are installed
    ;;
  *)
    log_error "swift/setup-toolset.sh: Unsupported host OS"
    return 1
    ;;
esac

# toolset
export HAM_TOOLSET=SWIFT
export HAM_TOOLSET_NAME=swift
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/${HAM_TOOLSET_NAME}"
export HAM_CPP_TOOLSET=$HAM_TOOLSET
export HAM_CPP_TOOLSET_NAME=$HAM_TOOLSET_NAME

if ! VER="--- swift ------------------------
$(swiftc --version 2>/dev/null)"; then
  log_error "swift/setup-toolset.sh: Can't get swiftc version."
  return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
