#!/bin/bash
toolset_import_once xslt_tools || return 1

# toolset
export HAM_TOOLSET=IOS
export HAM_TOOLSET_NAME=ios
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/ios"
export HAM_CPP_TOOLSET=$HAM_TOOLSET
export HAM_CPP_TOOLSET_NAME=$HAM_TOOLSET_NAME

# path setup
case $HAM_OS in
  OSX)
    ham-brew-install fastlane "bin/fastlane"
    ham-brew-install ios-deploy "bin/ios-deploy"
    ;;
  *)
    echo "E/Toolset: Unsupported host OS"
    return 1
    ;;
esac

export HAM_TARGET_BIN_LOA=ios-arm64

VER="--- ios ----------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  VER="$VER
$(clang --version)"
  if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
  fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
