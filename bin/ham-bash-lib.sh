#!/bin/bash
if [[ -z $HAM_HOME ]]; then
  echo "E/HAM_HOME not set !"
  exit 1
fi
if [[ -z $USERNAME ]]; then
  export USERNAME=$USER
fi
if [[ -z $USER ]]; then
  export USER=$USERNAME
fi
if [[ -z $EDITOR ]]; then
  export EDITOR=ham-editor
fi
if [[ -z "$HAM_TOOLSET_DL_URL" ]]; then
  export HAM_TOOLSET_DL_URL=https://f001.backblazeb2.com/file/cdn-ts/toolsets
fi

if [[ $OS == Windows* ]]; then
  # Serious BS from Cygwin...
  export CYGWIN=nodosfilewarning
  if [ -n "$VisualStudioVersion" ]; then
    # Inside visual studio, no colors...
    export TERM_NCOLORS=0
  else
    # On Windows both Emacs & the regular terminal seem to work fine with 8 colors...
    export TERM_NCOLORS=8
  fi
else
  export TERM_NCOLORS=${TERM_NCOLORS:-0}
  if [ "$TERM_NCOLORS" -eq 0 ]; then
    case "$TERM" in
      xterm-color | *-256color) TERM_NCOLORS=256 ;;
      xterm | *-88color) TERM_NCOLORS=88 ;;
      *)
        # Unknown, stays at zero
        ;;
    esac
  fi
fi

########################################################################
##  LOG
########################################################################

# Check if the terminal supports colors
if [ "$TERM_NCOLORS" -gt 0 ]; then
  HAM_TERMINAL_SUPPORTS_COLORS=true
else
  HAM_TERMINAL_SUPPORTS_COLORS=false
fi

log_info() {
  if [ "$HAM_TERMINAL_SUPPORTS_COLORS" = true ] && [ -z "$NO_COLOR" ]; then
    echo -e "\033[36mI/$*\033[0m"
  else
    echo "I/$*"
  fi
}

log_success() {
  if [ "$HAM_TERMINAL_SUPPORTS_COLORS" = true ] && [ -z "$NO_COLOR" ]; then
    echo -e "\033[32mS/$*\033[0m"
  else
    echo "S/$*"
  fi
}

log_error() {
  if [ "$HAM_TERMINAL_SUPPORTS_COLORS" = true ] && [ -z "$NO_COLOR" ]; then
    echo >&2 -e "\033[31mE/$*\033[0m"
  else
    echo >&2 "E/$*"
  fi
}

log_warning() {
  if [ "$HAM_TERMINAL_SUPPORTS_COLORS" = true ] && [ -z "$NO_COLOR" ]; then
    echo >&2 -e "\033[33mW/$*\033[0m"
  else
    echo >&2 "W/$*"
  fi
}

log_debug() {
  if [ "$HAM_TERMINAL_SUPPORTS_COLORS" = true ] && [ -z "$NO_COLOR" ]; then
    echo -e "\033[90mD/$*\033[0m"
  else
    echo "D/$*"
  fi
}

########################################################################
##  Tables
########################################################################
ni-hput() {
  eval export "NI_HASH_$1__""$2"='$3'
}

ni-hremove() {
  eval unset "NI_HASH_$1__""$2"
}

ni-hclear() {
  for h in $(ni-hkeys t); do
    ni-hremove "$1" "$h"
  done
}

ni-hget() {
  eval echo '${'"NI_HASH_$1__$2"'#hash}'
}

ni-hkeys() {
  env | grep -E "^NI_HASH_$1__[[:alnum:]]*=" | sed -r "s/^NI_HASH_$1__(.*)=.*/\\1/g"
}

ni-hprint() {
  env | grep -E "^NI_HASH_$1__[[:alnum:]]*=" | sed "s/^NI_HASH_$1__//g"
}

########################################################################
##  Utils
########################################################################
# usage:
#   complain ModuleName "Diagnostic Message"
#   complain "Diagnostic Message"
complain() {
  if [ "$1" = "_" ]; then
    shift
  fi
  if [ -z "$2" ]; then
    log_error "$1"
  else
    log_error "$1: $2"
  fi
}

# usage: die_exit ModuleName "Message Saying Why"
die_exit() {
  complain "$@"
  exit 1
}

# usage: die ModuleName "Message Saying Why"
die() {
  complain "$@"
  if [ -z "$HAM_DIE_SHOULD_RETURN" ]; then
    # echo "I/DIE: EXIT"
    exit 1
  else
    # echo "I/DIE: RETURN"
    return 1
  fi
}

# usage:
#   errcheck errorcode ModuleName "Message Saying Why"
#   errcheck $? $HAM_TOOLSET_NAME "Message Saying Why" || return 1
errcheck() {
  if [ "$1" != 0 ]; then
    die "$2" "$3 (errcode $1)"
  fi
}

retcheck() {
  if [ "$1" != "0" ]; then
    shift
    complain "$@"
    return 1
  fi
}

#
# check_file PATH (nope)
#
# 'nope' can be used to skip file check without writting a bunch of tests. For
# example you can set the PATH to 'nocheck' and pass 'nocheck' to nope and the
# check will be skipped instead of failing. This is useful when the list of
# files is built in the script before the checks are run, you can write
# 'nocheck' in the list and know that the check will be skipped afterward.
#
# return 1 if the file doesn't exist
#
check_file() {
  PATH=$1
  NOPE=$2
  # echo "... check_file: $NOPE :: $PATH"

  if [ -n "$NOPE" ] && [ "$NOPE" == "$PATH" ]; then
    # Nocheck
    echo "ok-nocheck"
    return 0
  elif [ -e "$PATH" ]; then
    # File exists
    echo "ok-exists"
    return 0
  else
    # File doesnt exists
    echo "notfound"
    return 1
  fi
}

# usage:
#   errcheck_file ModuleName FILE_PATH
#   errcheck_file $HAM_TOOLSET_NAME some_file_path.lib || return 1
errcheck_file() {
  MODULE_NAME="$1"
  FILE_PATH="$2"
  CHECK_FILE=$(check_file "$FILE_PATH")
  if [ "$CHECK_FILE" == "notfound" ]; then
    die "$MODULE_NAME" "Check file failed: $CHECK_FILE: '$FILE_PATH'."
  fi
}

# usage: NativePathNameVariable=$(nativedir "/directory/PathName")
nativedir() {
  DIRPATH="$1"
  case $HAM_OS in
    NT*)
      cd 2>/dev/null "$DIRPATH" || return 1
      pwd -W
      ;;
    *)
      (cd "$DIRPATH" && pwd)
      ;;
  esac
}

# usage: NativePathNameVariable=$(unxpath "/directory/PathName")
#
# Determine the native path name equivalent for a POSIX style
# "/directory/PathName"; (CAVEAT: the specified "/directory/PathName" *must*
# reference an existing *directory* on the MSYS or POSIX host).
unxpath() {
  DIR=$(nativedir "$1")
  case $HAM_OS in
    NT*)
      BLA=${DIR//\\/\/}
      if test "${BLA//[a-zA-Z]:*/ABSWINPATH}" = "ABSWINPATH"; then
        echo /"${BLA//:\//\/}"
      else
        echo "$BLA"
      fi
      ;;
    *)
      ABSPATH=${DIR}
      echo "$ABSPATH"
      ;;
  esac
}

abspath() {
  filename=$(basename "$1")
  filenamenoext=${filename%.*}
  ext=${filename##*.}
  if [[ "$ext" == "$filenamenoext" ]]; then
    filename="${filenamenoext}"
  else
    filename="${filenamenoext}.${ext}"
  fi
  dirname=$(dirname "$1")
  dir=$(nativedir "$dirname")
  echo "$dir/$filename"
}

path_filename() {
  filename=$(basename "$1")
  filename="${filename%.*}.${filename##*.}"
  echo "$filename"
}

path_filename_noext() {
  filename=$(basename "$1")
  echo "${filename%.*}"
}

path_extension() {
  filename=$1
  extension="${filename##*.}"
  if [[ "$extension" == "$filename" ]]; then
    echo ''
  else
    echo "$extension"
  fi
}

path_dirname() {
  dir=$(dirname "$1")
  echo "$dir"
}

path_abs_dirname() {
  dir=$(nativedir "$(dirname "$1")")
  echo "$dir"
}

path_native_dirname() {
  dir=$(nativedir "$(dirname "$1")")
  echo "$dir"
}

path_unix_dirname() {
  dir=$(unxpath "$(dirname "$1")")
  echo "$dir"
}

pathenv_add() {
  if [ ! -d "$1" ]; then
    return 0
  fi
  DIR=$(unxpath "$1")
  if [ -z "$PATH" ]; then
    PATH=$DIR
  elif [ -d "$DIR" ] && [[ ":$PATH:" != *":$DIR:"* ]]; then
    if [ "$2" = "after" ]; then
      export PATH=$PATH:$DIR
    else
      export PATH=$DIR:$PATH
    fi
  fi
}

pathenv_remove() {
  local D=":${PATH}:"
  [ "${D/:$1:/:}" != "$D" ] && PATH="${D/:$1:/:}"
  PATH="${PATH/#:/}"
  export PATH="${PATH/%:/}"
}

pathenv_remove_add() {
  pathenv_remove "$1"
  pathenv_add "$1" "$2"
}

current_git_branch() {
  git branch --no-color 2>/dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1) /'
}

ham_prompt_project_name() {
  if [ -z "$HAM_PROJECT_DIR" ]; then
    PROJECT_NAME=$(basename "$WORK")
  else
    PROJECT_NAME=$(basename "$HAM_PROJECT_DIR")
  fi
  echo "$PROJECT_NAME"
}

ham_prompt_build_info() {
  local USERTAG
  if [ -z "$STY" ]; then
    USERTAG=$USERNAME
  else
    USERTAG="$USERNAME in $STY"
  fi

  local BIN_LOA
  BIN_LOA=$(toolset_get_target_bin_loa "${BUILD_TARGET}")
  if [ "$BUILD_JNI" == "1" ]; then
    BIN_LOA="$BIN_LOA (jni)"
  fi

  local BUILD
  BUILD=${BUILD:-ra}

  echo "$USERTAG,$BIN_LOA,$BUILD"
}

update_prompt() {
  if test "$TERM_NCOLORS" -ge 8; then
    PROMPT="\[\033[35m\$(ham_prompt_project_name)\033[0m\] \[\033[32m\$(current_git_branch)\033[0m\]\w (\e[38;5;95m\$(ham_prompt_build_info)\e[0m) \[\033[0;36m\${HAM_IMPORTED_TOOLSETS}\033[0m\]"
  else
    PROMPT="# \$(ham_prompt_project_name) \[\$(current_git_branch)\]\w (\$(ham_prompt_build_info)) \${HAM_IMPORTED_TOOLSETS}"
  fi

  export PS1="
$PROMPT
$ "
}

upsearch() {
  test / == "$PWD" && return || test -e "$1" && echo "$PWD" && return || cd .. && upsearch "$1"
}

rawurlencode() {
  local string="${1}"
  local strlen=${#string}
  local encoded=""

  for ((pos = 0; pos < strlen; pos++)); do
    c=${string:$pos:1}
    case "$c" in
      [-_.~a-zA-Z0-9]) o="${c}" ;;
      *) printf -v o '%%%02x' "'$c" ;;
    esac
    encoded+="${o}"
  done
  echo "${encoded}"  # You can either set a return variable (FASTER)
  REPLY="${encoded}" #+or echo the result (EASIER)... or both... :p
}

ham_shell_set_title() {
  if [ -n "$ALACRITTY_LOG" ]; then
    echo -ne "\e]0;$1\007"
  fi
}

########################################################################
##  Toolsets
########################################################################

# dl_file output_file url
dl_file() {
  ham-dl-file "$1" "$2"
}

toolset_import() {
  ALREADY_IMPORTED=$(ni-hget HAM_IMPORTS_TOOLSETS "$1")
  if [[ "$ALREADY_IMPORTED" = "1" ]]; then
    if [[ "$HAM_OS" == NT ]] && [[ "${GITHUB_ACTIONS}" == "true" ]]; then
      # For some reason Windows GitHub Actions are messed up, we must
      # reimport to make sure the environment is properly setup
      echo "W/toolset_import: toolset already imported '$1', be reimported as we are in a Windows Github Action."
      . ham-toolset-do-import.sh force "$1" || return 1
    else
      echo "W/toolset_import: toolset already imported '$1', skipped."
    fi
  else
    . ham-toolset-do-import.sh "$1" || return 1
  fi
}

# This is meant to be used in _ham_project, do not use in setup-toolset.sh.
toolset_import_list() {
  for ARG in "$@"; do
    toolset_import "$ARG" || return 1
    export HAM_IMPORTED_TOOLSET=$ARG
  done
}

toolset_import_once() {
  ALREADY_IMPORTED=$(ni-hget HAM_IMPORTS_TOOLSETS "$1")
  if [[ $ALREADY_IMPORTED != "1" ]]; then
    . ham-toolset-do-import.sh "$1"
  fi
}

toolset_import_force() {
  . ham-toolset-do-import.sh force "$1"
}

toolset_import_strict() {
  . ham-toolset-do-import.sh "$1"
}

toolset_unquarantine_dir() {
  case $HAM_OS in
    OSX*)
      echo "I/Fixing macOS quarantine..."
      sudo xattr -r -d com.apple.quarantine "$1" || true
      ;;
  esac
}

toolset_dl_and_extract() {
  DL_DIR="${HAM_HOME}/toolsets/_dl"
  DIR="${HAM_HOME}/toolsets/$1"
  ARCH_URL="${HAM_TOOLSET_DL_URL}/$2.7z"
  # export ARCH_URL="http://localhost:8123/data/toolsets/$2.7z"
  DLFILENAME="_$2.7z"
  echo "=== Importing toolset '$1' from $DIR"

  mkdir -p "${DIR}"
  if ! pushd "${DIR}" >/dev/null; then
    echo "E/Can't go to the toolset directory '$DIR'."
    return 1
  fi

  if [ -e "$DL_DIR/$2.7z" ]; then
    echo "I/Copying archive found at '$DL_DIR/$2.7z'"
    (
      set -x
      cp "$DL_DIR/$2.7z" "$DLFILENAME"
    )
  fi

  if [ -e "$DLFILENAME" ]; then
    echo "I/Extracting '$DLFILENAME' ..."
    7z x -y "$DLFILENAME" | grep -v -e "\(7-Zip\|Processing\|Extracting\|^$\)" -
    if [ "${PIPESTATUS[0]}" != 0 ]; then
      echo "E/Extraction failed ! Removing corrupted archive, please re-run the environment setup."
      rm "$DLFILENAME"
      popd || return 1
      return 1
    fi
    popd || return 1
  elif [ ! -e "$DLFILENAME" ]; then
    echo "I/Trying download from ${ARCH_URL}"
    if ! dl_file "$DLFILENAME.dlfile" "$ARCH_URL"; then
      echo "E/Download failed !"
      popd || return 1
      return 1
    fi
    mv "$DLFILENAME.dlfile" "$DLFILENAME"
    echo "I/Extracting '$DLFILENAME' ..."
    7z x -y "$DLFILENAME" | grep -v -e "\(7-Zip\|Processing\|Extracting\|^$\)" -
    if [ "${PIPESTATUS[0]}" != 0 ]; then
      echo "E/Extraction failed ! Removing corrupted archive, please re-run the environment setup."
      rm "$DLFILENAME"
      popd || return 1
      return 1
    fi
    popd || return 1
  fi

  toolset_unquarantine_dir "${DIR}"
  echo "I/Done, downloaded and extracted toolset '$1'."
}

toolset_dl_cleanup() {
  export DLFILENAME="${HAM_HOME}/toolsets/$1/_$2.7z"
  if [ -e "$DLFILENAME" ]; then
    echo "I/Removing downloaded archive: $DLFILENAME"
    rm "$DLFILENAME"
  fi
}

toolset_info() {
  echo "=== Ham Info ======================================"
  # shellcheck disable=SC2153
  echo "TOOLSETS = ${HAM_IMPORTED_TOOLSETS}"
  echo "MAIN TOOLSET = ${HAM_TOOLSET}, NAME: ${HAM_TOOLSET_NAME}, DIR: ${HAM_TOOLSET_DIR}"
  echo -n "TOOLS VERSION = "
  echo "$HAM_TOOLSET_VERSIONS"
  echo "==================================================="
}

toolset_is_imported() {
  ni-hget HAM_IMPORTS_TOOLSETS "$1"
}

toolset_check_imported() {
  for ARG in "$@"; do
    if [[ -z "$(toolset_is_imported "$ARG")" ]]; then
      echo "E/'$ARG' toolset not imported. Did you forget to run '. hat' ?"
      return 1
    fi
  done
}

toolset_bak_mv() {
  if [ -e "$1" ]; then
    mv -f "$1" "$1__bak"
  fi
}

toolset_bak_rm() {
  if [ -e "$1__bak" ]; then
    rm -Rf "$1__bak"
  fi
}

# usage: toolset_dl TOOLSET_NAME TOOLSET_DL_NAME
toolset_dl() {
  toolset_dl_and_extract "$@"
  toolset_dl_cleanup "$@"
}

toolset_ver_file_path() {
  TS_NAME=$1
  TS_LOA=$2
  TS_VER=$3
  TS_DIR="${HAM_HOME}/toolsets/${TS_NAME}/${TS_LOA}"
  TS_VER_NAME=${TS_NAME}_${TS_LOA}_${TS_VER}
  TS_VER_FILE_NAME=toolset_${TS_VER_NAME}
  echo "${TS_DIR}/${TS_VER_FILE_NAME}"
}

# usage: toolset_check_ver name loa version
# example: toolset_check_ver repos nt-x86 v2
toolset_check_ver() {
  TS_NAME=$1
  TS_LOA=$2
  TS_VER=$3
  TS_DIR="${HAM_HOME}/toolsets/${TS_NAME}/${TS_LOA}"
  TS_VER_NAME=${TS_NAME}_${TS_LOA}_${TS_VER}
  TS_VER_FILE_NAME=toolset_${TS_VER_NAME}
  if [ ! -e "${TS_DIR}/${TS_VER_FILE_NAME}" ]; then
    log_error "Toolset '$TS_NAME': Can't find '${TS_VER_FILE_NAME}' in '${TS_DIR}'."
    return 1
  else
    return 0
  fi
}

# usage: toolset_check_and_dl_ver name loa version
# example: toolset_check_and_dl_ver repos nt-x86 v2
toolset_check_and_dl_ver() {
  TS_NAME=$1
  TS_LOA=$2
  TS_VER=$3
  TS_DIR="${HAM_HOME}/toolsets/${TS_NAME}/${TS_LOA}"
  TS_VER_NAME=${TS_NAME}_${TS_LOA}_${TS_VER}
  TS_VER_FILE_NAME=toolset_${TS_VER_NAME}
  if [ ! -e "${TS_DIR}/${TS_VER_FILE_NAME}" ]; then
    toolset_bak_mv "${TS_LOA}"
    toolset_dl_and_extract "${TS_NAME}" "${TS_VER_NAME}"
    if [ ! -e "${TS_DIR}/${TS_VER_FILE_NAME}" ]; then
      echo "E/Toolset '$TS_NAME': Can't find '${TS_VER_FILE_NAME}' in '${TS_DIR}'."
      return 1
    fi
    toolset_dl_cleanup "${TS_NAME}" "${TS_VER_NAME}"
    toolset_bak_rm "${TS_LOA}"
  else
    toolset_dl_cleanup "${TS_NAME}" "${TS_VER_NAME}"
  fi
}

toolset_get_target_bin_loa() {
  BUILD_TARGET=${1:-$BUILD_TARGET}
  if [ -z "$BUILD_TARGET" ]; then
    BUILD_TARGET=${HAM_TARGET_BIN_LOA:-default}
  fi
  if [ "$BUILD_TARGET" = "default" ]; then
    # Should mirror the default toolsets in toolsets/default/setup-toolset.sh
    case $HAM_OS in
      NT*)
        BUILD_TARGET=nt-x64
        ;;
      OSX*)
        if [ "$HAM_BIN_LOA" == "osx-arm64" ]; then
          BUILD_TARGET=osx-arm64
        else
          BUILD_TARGET=osx-x64
        fi
        ;;
      LINUX)
        BUILD_TARGET=lin-x64
        ;;
      *)
        echo "E/toolset_get_target_bin_loa: Unsupported host OS '$HAM_OS'"
        return 1
        ;;
    esac
  fi
  if [ -z "$BUILD_TARGET" ]; then
    echo "E/toolset_get_target_bin_loa: Can't determine BUILD_TARGET."
    return 1
  fi
  echo "$BUILD_TARGET"
}

########################################################################
##  Utils
########################################################################
iso_date() {
  date +"%Y-%m-%dT%H:%M:%S%z"
}

iso_datez() {
  date -u +"%Y-%m-%dT%H:%M:%SZ"
}

tag_date() {
  date +"%Y_%m_%dT%H_%M_%S%z"
}

tag_datez() {
  date -u +"%Y_%m_%dT%H_%M_%SZ"
}

arch_date() {
  date +"%Y%m%d_%H%M%S"
}

arch_datez() {
  date -u +"%Y%m%d_%H%M%SZ"
}

ver_date() {
  date +"v%y_%m_%d"
}

fcd() {
  local dir
  while true; do
    # exit with ^D
    # shellcheck disable=SC2010
    dir="$(ls -a1F | grep '[/@]$' | grep -v '^./$' | sed 's/@$//' | fzf --height 40% --reverse --no-multi --preview 'pwd' --preview-window=up,1,border-none --no-info)"
    if [[ -z "${dir}" ]]; then
      break
    elif [[ -d "${dir}" ]]; then
      cd "${dir}" || return 1
    fi
  done
}

#
# history_cleanup <histfile>
#
# Clean up and deduplicate a history file, removing merge markers and empty lines.
# Creates a backup as <histfile>.bak before modifying.
#
# Returns:
#   0 on success
#   1 if history file does not exist
#
history_cleanup() {
  local filepath="$1"

  if [ ! -f "${filepath}" ]; then
    log_warning "history_cleanup: History file does not exist: ${filepath}"
  else
    sed '/^<<</{d;};/^>>>/{d;};/^===/{d;}' "${filepath}" |
      sort -f -u |
      sed '/^[[:space:]]*$/d' >"${filepath}.new" &&
      mv "${filepath}" "${filepath}.bak" &&
      mv "${filepath}.new" "${filepath}"
  fi
}

#
# fhh (print)
#
# Note: Can't be in an external script because each bash script has an
# independant (empty) history by default.
#
fhh() {
  if [ -z "$HISTFILE" ]; then
    echo "E/No history file defined."
  else
    CMD=$(
      history |
        # get the history item, skip the first column which is the timestamp
        awk '{$1="";print substr($0,2)}' |
        # exclude the fhh command itself
        grep -v '^fhh$' |
        # invert the list so that we get the most recent above the prompt
        tac |
        # remove duplicate entries
        awk '!/./ || !seen[$0]++' |
        # run fzf
        fzf --expect=tab
    )
    KEY=$(echo "${CMD}" | head -1)
    CMD=$(echo "${CMD}" | tail -1)
    # echo "I/KEY: $KEY"
    # echo "I/COMMAND: $CMD"
    if [ "$1" == "print" ] || [ -n "$KEY" ]; then
      # echo "I/Print"
      echo "${CMD}"
    else
      # echo "I/Execute"
      echo "$ ${CMD}"
      history -s "$CMD"
      eval "${CMD}"
    fi
  fi
}

#
# tagfile_status tagfile tag
#
# echo's "up-to-date" or "outdated" or "no_tagfile"
#
# example usage:
#   if [[ `tagfile_status "$TAGFILE" "$TAG"` == "up-to-date" ]]; then
#     echo "I/$TAGFILE is up-to-date."
#   else
#     echo "I/$TAGFILE *is not* up-to-date."
#   endif
#
tagfile_status() {
  TAGFILE=$1
  TAG=$2
  if [ -e "$TAGFILE" ]; then
    PREVTAG=$(cat "$TAGFILE")
    CURRTAG="$TAG"
    if [ "$PREVTAG" == "$CURRTAG" ]; then
      # tagfile is up-to-date
      echo "up-to-date"
    else
      # tagfile is outdated
      echo "outdated"
    fi
  else
    # tagfile doesn't exist
    echo "no_tagfile"
  fi
}

# tagfile_update tagfile tag
tagfile_update() {
  OUTFILE=$1
  TAG=$2
  mkdir -p "$(dirname "$OUTFILE")"
  echo "$TAG" >"$OUTFILE"
  echo "I/Updated tagfile: '$OUTFILE'"
}

ham_find_which_path() {
  case $HAM_OS in
    NT*)
      local path
      path=$("type" -p "$*" 2>/dev/null)
      echo -n "$path"
      ;;
    *)
      local path
      path=$("which" "$*" 2>/dev/null)
      echo -n "$path"
      ;;
  esac
}

########################################################################
##  OS Packages
########################################################################
ham_os_package_exist_pacman() {
  pacman -Si "$1" &>/dev/null && return 0
  return 1
}

ham_os_package_exist_apt() {
  apt-cache show "$1" &>/dev/null && return 0
  return 1
}

ham_os_package_exist_brew() {
  brew info "$1" &>/dev/null && return 0
  return 1
}

ham_os_package_install_pacman() {
  local FUNC_NAME="ham_os_package_install_pacman"
  local PACKAGE_NAME=$1
  if [ -z "$PACKAGE_NAME" ]; then
    complain "$FUNC_NAME" "PACKAGE_NAME not specified"
    return 1
  fi

  (
    set -ex
    sudo pacman -q -S --noconfirm "$PACKAGE_NAME"
  )
}

ham_os_package_install_apt() {
  local FUNC_NAME="ham_os_package_install_apt"
  local PACKAGE_NAME=$1
  if [ -z "$PACKAGE_NAME" ]; then
    complain "$FUNC_NAME" "PACKAGE_NAME not specified"
    return 1
  fi

  (
    set -ex
    ham-apt-get-install "$PACKAGE_NAME"
  )
}

ham_os_package_install_brew() {
  local FUNC_NAME="ham_os_package_install_brew"
  local PACKAGE_NAME=$1
  if [ -z "$PACKAGE_NAME" ]; then
    complain "$FUNC_NAME" "PACKAGE_NAME not specified"
    return 1
  fi

  (
    set -ex
    ham-brew install "$PACKAGE_NAME"
  )
}

ham_os_package_check_and_install_pacman() {
  local FUNC_NAME="ham_os_package_check_and_install_pacman"
  local PACKAGE_NAME=$1
  if [ -z "$PACKAGE_NAME" ]; then
    complain "$FUNC_NAME" "PACKAGE_NAME not specified"
    return 1
  fi

  if pacman -Qi "$PACKAGE_NAME" &>/dev/null; then
    log_info "Package '$PACKAGE_NAME' is already installed."
  else
    log_info "Package '$PACKAGE_NAME' is not installed. Installing..."
    ham_os_package_install_pacman "$PACKAGE_NAME"
  fi
}

ham_os_package_check_and_install_apt() {
  local FUNC_NAME="ham_os_package_check_and_install_apt"
  local PACKAGE_NAME=$1
  if [ -z "$PACKAGE_NAME" ]; then
    complain "$FUNC_NAME" "PACKAGE_NAME not specified"
    return 1
  fi

  if dpkg -l | grep -q "^ii.*$PACKAGE_NAME "; then
    log_info "Package '$PACKAGE_NAME' is already installed."
  else
    log_info "Package '$PACKAGE_NAME' is not installed. Installing..."
    ham_os_package_install_apt "$PACKAGE_NAME"
  fi
}

ham_os_package_check_and_install_brew() {
  local FUNC_NAME="ham_os_package_check_and_install_brew"
  local PACKAGE_NAME=$1
  if [ -z "$PACKAGE_NAME" ]; then
    complain "$FUNC_NAME" "PACKAGE_NAME not specified"
    return 1
  fi

  if ls "$(ham-brew-installdir "$1")"/ &>/dev/null; then
    echo "Package '$PACKAGE_NAME' is already installed."
  else
    echo "Package '$PACKAGE_NAME' is not installed. Installing..."
    ham_os_package_install_brew "$PACKAGE_NAME"
  fi
}

#
# usage: ham_os_package_find_in_usr_dir TYPE FILE
#   TYPE: One of 'lib', 'include', or 'bin', representing the type of file to search for.
#   FILE: The name of the file to search for. Can include directory structure (e.g., 'curl/curl.h' or 'libcurl.so').
# Example:
#   ham_os_package_find_in_usr_dir include "curl/curl.h"  # Search for 'curl/curl.h' in include paths.
#   ham_os_package_find_in_usr_dir lib "libcurl.so"       # Search for 'libcurl.so' in library paths.
#   ham_os_package_find_in_usr_dir bin "curl"             # Search for the 'curl' binary in bin paths.
#
ham_os_package_find_in_usr_dir() {
  local FUNC_NAME="ham_os_package_find_in_usr_dir"
  local TYPE=$1
  if [ -z "$TYPE" ]; then
    complain "$FUNC_NAME" "TYPE not specified"
    return 1
  fi
  local FILE=$2
  if [ -z "$FILE" ]; then
    complain "$FUNC_NAME" "FILE not specified"
    return 1
  fi

  # Search for the file in a few directories
  SEARCH_DIRS=("/usr/$TYPE" "/usr/local/$TYPE")

  if [[ "$HAM_BIN_LOA" == "lin-x64" && "$HAM_OS_PACKAGE_MANAGER" == "apt" ]]; then
    ARCH_USR_DIR="/usr/$TYPE/x86_64-linux-gnu"
    if [[ -d "$ARCH_USR_DIR" ]]; then
      # Append to SEARCH_DIRS
      SEARCH_DIRS+=("$ARCH_USR_DIR")
    fi
  fi

  for DIR in "${SEARCH_DIRS[@]}"; do
    local TRY_PATH="$DIR/$FILE"
    if [[ -e "$TRY_PATH" ]]; then
      echo "$TRY_PATH"
      return 0
    fi
  done

  # empty value if we cant find the file, no error code
  return 0
}

#
# usage: ham_os_package_get_value KEYNAME (KEYNAME:VALUE) (DEFAULT_VALUE)
#
# Return an error if no DEFAULT_VALUE is specified and we can't find a
# matching value for the key specified.
#
# Example:
#   ham_os_package_get_value $KEYNAME apt:binutils-dev binutils
#   ham_os_package_get_value $KEYNAME apt:libsdl2-dev sdl2
#   ham_os_package_get_value $KEYNAME pacman:squirrel apt:foo_for_apt brew:foodebiere
#
ham_os_package_get_value() {
  local FUNC_NAME="ham_os_package_get_value"
  local KEYNAME=$1
  if [ -z "$KEYNAME" ]; then
    complain "$FUNC_NAME" "KEYNAME not specified"
    return 1
  fi
  shift

  local args=("$@")

  # Look for the default value first so that we can validate how the default
  # values are specified
  local DEFAULT_VALUE=""
  for MAPPING in "${args[@]}"; do
    # If the argument does not contain a colon, it's the default value
    if [[ "$MAPPING" != *":"* ]]; then
      if [[ -n "$DEFAULT_VALUE" ]]; then
        complain "$FUNC_NAME" "Multiple default values specified."
        return 1
      fi
      DEFAULT_VALUE=$MAPPING
      continue
    fi
  done

  for MAPPING in "${args[@]}"; do
    # Extract key and value from the mapping
    local MAPPED_KEY
    MAPPED_KEY=$(echo "$MAPPING" | cut -d':' -f1)
    local MAPPED_VALUE
    MAPPED_VALUE=$(echo "$MAPPING" | cut -d':' -f2)

    # If the current mapping matches the key, return the value
    if [[ "$KEYNAME" == "$MAPPED_KEY" ]]; then
      echo "$MAPPED_VALUE"
      return 0
    fi
  done

  # If no specific key match and DEFAULT_VALUE is specified, return it
  if [[ -n "$DEFAULT_VALUE" ]]; then
    echo "$DEFAULT_VALUE"
    return 0
  fi

  # If no match and no DEFAULT_VALUE, return an error
  complain "$FUNC_NAME" "No matching value for key '$KEYNAME' and no default value provided."
  return 1
}

# Detect the default package manager in the current environment
ham_os_package_detect_default_manager() {
  local FUNC_NAME="ham_os_package_detect_default_manager"
  local PKGMANAGER=""

  # Check for Homebrew. Prioritize it on macOS or if experimental Linuxbrew is enabled.
  if command -v brew &>/dev/null; then
    PKGMANAGER="brew"
    if [[ "$HAM_ENABLE_EXPERIMENTAL_LINUX_BREW" == "1" || "$OSTYPE" == "darwin"* ]]; then
      echo "$PKGMANAGER"
      return 0
    fi
  fi

  # Check for Pacman (Arch Linux / SteamOS)
  if command -v pacman &>/dev/null; then
    PKGMANAGER="pacman"
  # Check for APT (Debian/Ubuntu)
  elif command -v apt-get &>/dev/null; then
    PKGMANAGER="apt"
  fi

  if [[ -n "$PKGMANAGER" ]]; then
    echo "$PKGMANAGER"
  else
    echo "none"
  fi
}

#
# usage: ham_os_package_syslib_find_file TYPE FILE [OS PACKAGES]
#   TYPE: One of 'lib', 'include', or 'bin', representing the type of file to search for.
#   FILE: The name of the file to search for. Can include directory structure (e.g., 'curl/curl.h' or 'libcurl.so').
#   OS PACKAGES: A list of package options like: apt:libcurl4-openssl-dev curl
#
function ham_os_package_syslib_find_file() {
  local FUNC_NAME="ham_os_package_syslib_find_file"
  if [[ -z "$HAM_OS_PACKAGE_MANAGER" || "$HAM_OS_PACKAGE_MANAGER" == "none" ]]; then
    complain "$FUNC_NAME" "HAM_OS_PACKAGE_MANAGER is not set or is 'none'"
    return 1
  fi

  local TYPE="$1"
  if [[ -z "$TYPE" ]]; then
    complain "$FUNC_NAME" "TYPE is not specified"
    return 1
  fi
  shift

  local FILE="$1"
  if [[ -z "$FILE" ]]; then
    complain "$FUNC_NAME" "FILE is not specified"
    return 1
  fi
  shift

  local -a PACKAGE_OPTIONS=("$@")

  # If the package manager is brew, use ham-brew-installdir
  if [[ "$HAM_OS_PACKAGE_MANAGER" == "brew" ]]; then
    local BREWPKGNAME
    BREWPKGNAME=$(ham_os_package_get_value brew "${PACKAGE_OPTIONS[@]}")
    if [[ -z "$BREWPKGNAME" ]]; then
      return 0
    fi

    local INSTALL_DIR
    INSTALL_DIR=$(ham-brew-installdir "$BREWPKGNAME" "$TYPE")
    if [[ -z "$INSTALL_DIR" ]]; then
      return 0
    fi

    if [[ -f "$INSTALL_DIR/$FILE" ]]; then
      echo "$INSTALL_DIR/$FILE"
      return 0
    fi

    if [[ -n "$(ham_find_which_path xcrun)" ]]; then
      INSTALL_DIR=$(xcrun --show-sdk-path)/usr/$TYPE
      if [[ -f "$INSTALL_DIR/$FILE" ]]; then
        echo "$INSTALL_DIR/$FILE"
        return 0
      fi
    fi
  else
    # For other package managers, use ham_os_package_find_in_usr_dir
    ham_os_package_find_in_usr_dir "$TYPE" "$FILE"
    return 0
  fi

  # if its a binary we allow the path to work aswell
  if [[ "$TYPE" == "bin" ]]; then
    EXE_PATH=$(ham_find_which_path "$FILE")
    if [[ -e "$EXE_PATH" ]]; then
      echo "$EXE_PATH"
    fi
  fi
}

#
# usage: ham_os_package_syslib_check_and_install TYPE FILE [OS PACKAGES]
#   TYPE: One of 'lib', 'include', or 'bin', representing the type of file to search for.
#   FILE: The name of the file to search for. Can include directory structure (e.g., 'curl/curl.h' or 'libcurl.so').
#   OS PACKAGES: A list of package options like 'apt:libcurl4-openssl-dev pacman:curl brew:curl'
#
# return: 0 if succeeded and print the path of the file, 1 if failed and print an error message
#
function ham_os_package_syslib_check_and_install() {
  local FUNC_NAME="ham_os_package_syslib_check_and_install"
  if [[ -z "$HAM_OS_PACKAGE_MANAGER" || "$HAM_OS_PACKAGE_MANAGER" == "none" ]]; then
    complain "$FUNC_NAME" "HAM_OS_PACKAGE_MANAGER is not set or is 'none'"
    return 1
  fi

  local TYPE="$1"
  if [[ -z "$TYPE" ]]; then
    complain "$FUNC_NAME" "TYPE is not specified"
    return 1
  fi
  shift

  local FILE="$1"
  if [[ -z "$FILE" ]]; then
    complain "$FUNC_NAME" "FILE is not specified"
    return 1
  fi
  shift

  local -a PACKAGE_OPTIONS=("$@")

  local PACKAGE_NAME
  PACKAGE_NAME=$(ham_os_package_get_value "$HAM_OS_PACKAGE_MANAGER" "${PACKAGE_OPTIONS[@]}")
  if [[ -z "$PACKAGE_NAME" ]]; then
    complain "$FUNC_NAME" "Unable to find a valid package name from '${PACKAGE_OPTIONS[*]}'"
    return 1
  fi

  # Try to find the file first
  local FILE_PATH
  FILE_PATH=$(ham_os_package_syslib_find_file "$TYPE" "$FILE" "${PACKAGE_OPTIONS[@]}")
  if [[ -n "$FILE_PATH" ]]; then
    echo "$FILE_PATH"
    return 0
  fi

  # If not found, attempt to install the package
  log_info "'$FILE' not found, attempting to install..." 1>&2

  # Install the package based on the package manager
  case "$HAM_OS_PACKAGE_MANAGER" in
    pacman)
      ham_os_package_install_pacman "$PACKAGE_NAME" 1>&2
      ;;
    apt)
      ham_os_package_install_apt "$PACKAGE_NAME" 1>&2
      ;;
    brew)
      ham_os_package_install_brew "$PACKAGE_NAME" 1>&2
      ;;
    *)
      complain "$FUNC_NAME" "Unsupported package manager '$HAM_OS_PACKAGE_MANAGER'"
      return 1
      ;;
  esac

  # Check again if the file can be found after the installation
  FILE_PATH=$(ham_os_package_syslib_find_file "$TYPE" "$FILE" "${PACKAGE_OPTIONS[@]}")
  if [[ -n "$FILE_PATH" ]]; then
    echo "$FILE_PATH"
    return 0
  else
    complain "$FUNC_NAME" "Unable to find '$FILE' after installing package '$PACKAGE_NAME' with '$HAM_OS_PACKAGE_MANAGER'."
    return 1
  fi
}

########################################################################
##  Environments
########################################################################
# Set HAM_OS first, its used by the script commands
if [[ $OS == Windows* ]]; then
  export HAM_OS=NT
  export HAM_BIN_LOA=nt-x86
  if [ -z "$HOME" ]; then
    HOME=$(unxpath "$USERPROFILE")
    export HOME
  fi
elif [[ "$OSTYPE" == "darwin"* ]]; then
  export HAM_OS=OSX
  UNAME_STR=$(uname)
  MACHINE_TYPE=$(uname -m)
  if [ "$MACHINE_TYPE" == "x86_64" ]; then
    IS_TRANSLATED=$(
      sysctl -n sysctl.proc_translated 2>/dev/null
      true
    )
    if [ "$IS_TRANSLATED" == "1" ]; then
      # echo "W/!!! RUNNING UNDER ROSETTA, forcing HAM_BIN_LOA=osx-arm64 !!!"
      export HAM_BIN_LOA=osx-arm64
    else
      export HAM_BIN_LOA=osx-x64
    fi
  elif [ "$MACHINE_TYPE" == "arm64" ]; then
    export HAM_BIN_LOA=osx-arm64
  else
    echo "W/Unknown OS '$UNAME_STR' '$MACHINE_TYPE'"
  fi
  # export HAM_LOCAL_HOMEBREW="$HAM_HOME/toolsets/_brew/$HAM_BIN_LOA"
else
  UNAME_STR=$(/bin/uname)
  MACHINE_TYPE=$(/bin/uname -m)
  if [ "$UNAME_STR" == "Linux" ] && [ "$MACHINE_TYPE" == "x86_64" ]; then
    export HAM_OS=LINUX
    export HAM_BIN_LOA=lin-x64
    # export HAM_LOCAL_HOMEBREW="$HAM_HOME/toolsets/_brew/$HAM_BIN_LOA"
  else
    echo "W/Unknown OS '$UNAME_STR' '$MACHINE_TYPE'"
  fi
fi

if [[ -z $BUILD_BIN_LOA ]]; then
  export BUILD_BIN_LOA=$HAM_BIN_LOA
fi

if [[ -z "$WORK" ]]; then
  WORK=$(nativedir "$HAM_HOME/..")
  if [ "$HAM_NO_VER_CHECK" != "1" ]; then
    echo "W/WORK not set, set to '$WORK' by default."
  fi
fi
WORK=$(unxpath "$WORK")
export WORK

if [[ -z $TEMPDIR ]]; then
  export TEMPDIR=$WORK/_ham
fi

# Detect the default number of jobs
if [[ -z $HAM_NUM_JOBS ]]; then
  if [[ -n $NUMBER_OF_PROCESSORS ]]; then
    HAM_NUM_JOBS=$NUMBER_OF_PROCESSORS
  elif [[ -f /proc/cpuinfo ]]; then
    HAM_NUM_JOBS=$(grep -c processor /proc/cpuinfo)
  elif [[ "$OSTYPE" == "darwin"* ]]; then
    HAM_NUM_JOBS=$(sysctl -n machdep.cpu.thread_count)
  else
    HAM_NUM_JOBS=2
  fi
fi
export HAM_NUM_JOBS

if [[ -z "$HOME" ]]; then
  echo "E/HOME not set !"
  exit 1
fi
if [[ -z "$HAM_OS" ]]; then
  echo "E/HAM_OS not set !"
  exit 1
fi
if [[ -z "$HAM_BIN_LOA" ]]; then
  echo "E/HAM_BIN_LOA not set !"
  exit 1
fi
if [[ -z "$BUILD_BIN_LOA" ]]; then
  echo "E/BUILD_BIN_LOA not set !"
  exit 1
fi
