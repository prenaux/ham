#!/bin/bash
toolset_import_once xslt_tools || return 1

# We're using clang...
export LINUX_CLANG=1

# Use clang
export HAM_TOOLSET=CLANG
export HAM_TOOLSET_NAME=clang_18
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/${HAM_TOOLSET_NAME}"
export HAM_CPP_TOOLSET=$HAM_TOOLSET
export HAM_CPP_TOOLSET_NAME=$HAM_TOOLSET_NAME

case $HAM_OS in
  LINUX)
    toolset_check_and_dl_ver clang_18 lin-x64 v18_1_8 || return 1

    export CLANG_DIR="${HAM_TOOLSET_DIR}/lin-x64"
    export OSPLAT=X64
    export BUILD_BIN_LOA=$HAM_BIN_LOA

    if command -v pacman &>/dev/null; then
      if [ ! -f "/usr/lib/libtinfo.so.5" ]; then
        log_info "Library /usr/lib/libtinfo.so.5 does not exist. Installing ncurses5-compat-libs from local package."
        if sudo pacman -U "${CLANG_DIR}/deps/ncurses5-compat-libs-6.5-1-x86_64.pkg.tar.zst" --noconfirm; then
          log_info "ncurses5-compat-libs installed successfully."
        else
          log_error "Failed to install ncurses5-compat-libs."
          return 1
        fi
      fi
    fi
    ;;
  *)
    echo "E/Toolset: Unsupported host OS"
    return 1
    ;;
esac

pathenv_add "${CLANG_DIR}/bin"

# finding correct clang compiler dir
dir=$(clang --version | grep InstalledDir)
export CMD_JSON_COMPILER_PATH=${dir#*' '}/

if ! VER="--- linux_x64 -----------------------
$(clang -arch x86_64 --version)"; then
  echo "E/Can't get version."
  return 1
fi

export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"