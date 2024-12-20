#!/bin/bash
toolset_import_once xslt_tools || return 1
case $HAM_OS in
  NT)
    toolset_import_once msvc_19_x64 || return 1
    ;;
esac

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

    # fixup clang 18, it needs libtinfo.so.5 and ncurses5
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
    elif command -v apt-get &>/dev/null; then
      if [ ! -f "/usr/lib/x86_64-linux-gnu/libtinfo.so.5" ]; then
        log_info "Library libtinfo.so.5 does not exist. Installing ncurses5..."
        ham-apt-get-install libncurses5
        if [ ! -f "/usr/lib/x86_64-linux-gnu/libtinfo.so.5" ]; then
          log_error "Library libtinfo.so.5 still cant be found after installation."
          return 1
        fi
      fi
    fi
    ;;
  NT)
    toolset_check_and_dl_ver clang_18 nt-x64 v18_1_8 || return 1
    export CLANG_DIR="${HAM_TOOLSET_DIR}/nt-x64"
    export OSPLAT=X64
    export BUILD_BIN_LOA=$HAM_BIN_LOA
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

VER="--- clang_18 ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  if ! VER="$VER
$(clang -arch x86_64 --version)"; then
    echo "E/Can't get version."
    return 1
  fi
fi

export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
