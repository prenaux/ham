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

if [[ $OS == Windows* ]]; then
    # Serious BS from Cygwin...
    export CYGWIN=nodosfilewarning
    # On Windows both Emacs & the regular terminal seem to work fine with 8 colors...
    export TERM_NCOLORS=8
else
    if test -t 1; then
      if [[ -z "$TERM_NCOLORS" ]]; then
        export TERM_NCOLORS=$(tput colors)
      fi
    fi
    export TERM_NCOLORS=${TERM_NCOLORS:-0}
fi

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
        ni-hremove $1 $h
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
complain()
# usage:
#   complain ModuleName "Diagnostic Message"
#   complain "Diagnostic Message"
{
  if [ "$1" = "_" ]; then
      shift
  fi
  if [ -z "$2" ]; then
      echo >&2 "E/$1"
  else
      echo >&2 "E/$1: $2"
  fi
}

die_exit()
# usage: die_exit ModuleName "Message Saying Why"
{
  complain "$@"
  exit 1
}

die()
# usage: die ModuleName "Message Saying Why"
{
  complain "$@"
  if [ -z "$HAM_DIE_SHOULD_RETURN" ]; then
    # echo "I/DIE: EXIT"
    exit 1
  else
    # echo "I/DIE: RETURN"
    return 1
  fi
}

errcheck()
{
  if [ $1 != 0 ]
  then
    die $2 "$3 (errcode $1)"
  fi
}

retcheck()
{
  if [ "$1" != "0" ]; then
    shift
    complain "$@"
    return 1
  fi
}

nativedir()
# usage: NativePathNameVariable=$(nativedir "/directory/PathName")
{
    DIRPATH="$1"
    case $HAM_OS in
        NT*)
            2>/dev/null cd "$DIRPATH"; pwd -W
            ;;
        *)
            echo $( cd "$DIRPATH" && pwd )
            ;;
    esac
}

unxpath()
# usage: NativePathNameVariable=$(unxpath "/directory/PathName")
#
# Determine the native path name equivalent for a POSIX style
# "/directory/PathName"; (CAVEAT: the specified "/directory/PathName" *must*
# reference an existing *directory* on the MSYS or POSIX host).
{
    DIR=`nativedir "$1"`
    case $HAM_OS in
        NT*)
            BLA=${DIR//\\/\/}
            if test ${BLA//[a-zA-Z]:*/ABSWINPATH} = "ABSWINPATH"; then
                echo /${BLA//:\//\/}
            else
                echo $BLA
            fi
            ;;
        *)
            ABSPATH=${DIR}
            echo $ABSPATH
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
    dir=`nativedir "$dirname"`
    echo "$dir/$filename"
}

path_filename() {
    filename=$(basename "$1")
    filename="${filename%.*}.${filename##*.}"
    echo "$filename"
}

pathenv_add() {
    if [ ! -d "$1" ]; then
        return 0
    fi
    DIR=$(unxpath "$1")
    if [ -z "$PATH" ]; then
      PATH=$DIR
    elif [ -d "$DIR" ] && [[ ":$PATH:" != *":$DIR:"* ]] ; then
        if [ "$2" = "after" ] ; then
            export PATH=$PATH:$DIR
        else
            export PATH=$DIR:$PATH
        fi
    fi
}

pathenv_remove() {
    local D=":${PATH}:";
    [ "${D/:$1:/:}" != "$D" ] && PATH="${D/:$1:/:}";
    PATH="${PATH/#:/}";
    export PATH="${PATH/%:/}";
}

pathenv_remove_add() {
    pathenv_remove "$1"
    pathenv_add "$1" "$2"
}

current_git_branch() {
    git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1) /'
}

update_prompt() {
    TOOLSET_EXTRA=
    if [ "$BUILD_JNI" == "1" ]; then
        TOOLSET_EXTRA=" (jni)"
    fi

    if [ -z "$HAM_PROJECT_DIR" ]; then
        PROJECT_NAME=`basename "$WORK"`
    else
        PROJECT_NAME=`basename "$HAM_PROJECT_DIR"`
    fi

    if [ -z "$STY" ]; then
        USERTAG=$USERNAME
    else
        USERTAG="$USERNAME in $STY"
    fi

    BIN_LOA=${HAM_TARGET_BIN_LOA:-${HAM_BIN_LOA}}
    BUILD=${BUILD:-ra}

    if test "$TERM_NCOLORS" -ge 8; then
        PROMPT='\[\033[35m$PROJECT_NAME$TOOLSET_EXTRA\033[0m\] \[\033[32m$(current_git_branch)\033[0m\]\w (\e[38;5;95m$USERTAG\e[0m,\e[38;5;95m$BIN_LOA\e[0m,\e[38;5;95m$BUILD\e[0m) \[\033[0;36m$HAM_IMPORTED_TOOLSETS\033[0m\]'

        if [ ! -z "$DEVSERVER" ]; then
            PROMPT="$PROMPT, \[\033[32m$DEVSERVER\033[0m\]"
        fi

        if [ ! -z "$HOSTNAME_LABEL" ]; then
            PROMPT="\[\033[${HOSTNAME_COLOR:-37}m${HOSTNAME_LABEL}\033[0m\] $PROMPT"
        fi
    else
        PROMPT='# $PROJECT_NAME$TOOLSET_EXTRA \[$(current_git_branch)\]\w ($USERTAG,$BIN_LOA,$BUILD) $HAM_IMPORTED_TOOLSETS'
    fi

    export PS1="
$PROMPT
$ "
}

update_project_work() {
    export WORK="`unxpath $1`"
    echo "I/Updated WORK: '$WORK'"
}

upsearch() {
    test / == "$PWD" && return || test -e "$1" && echo "$PWD" && return || cd .. && upsearch "$1"
}

rawurlencode() {
  local string="${1}"
  local strlen=${#string}
  local encoded=""

  for (( pos=0 ; pos<strlen ; pos++ )); do
     c=${string:$pos:1}
     case "$c" in
        [-_.~a-zA-Z0-9] ) o="${c}" ;;
        * )               printf -v o '%%%02x' "'$c"
     esac
     encoded+="${o}"
  done
  echo "${encoded}"    # You can either set a return variable (FASTER)
  REPLY="${encoded}"   #+or echo the result (EASIER)... or both... :p
}

########################################################################
##  Toolsets
########################################################################

# dl_file output_file url
dl_file() {
    ham-dl-file "$1" "$2"
}

toolset_import() {
    export PATH=$PATH
    . ham-toolset-import.sh $1
    if [ $? != 0 ]; then
        return 1
    fi
}

toolset_import_once() {
    ALREADY_IMPORTED=`ni-hget HAM_IMPORTS_TOOLSETS $1`
    if [[ $ALREADY_IMPORTED = "1" ]]; then
        if [ "$2" != "silent" ]; then
            echo "I/Already imported '$1'."
        fi
    else
        . ham-toolset-import.sh $1
    fi
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
    export CWD=`pwd`
    export DL_DIR="${HAM_HOME}/toolsets/_dl"
    export DIR="${HAM_HOME}/toolsets/$1"
    export ARCH_URL="https://cdn2.talansoft.com/ftp/toolsets/$2.7z"
    # export ARCH_URL="http://localhost:8123/data/toolsets/$2.7z"
    export DLFILENAME="_$2.7z"
    echo "=== Importing toolset '$1' from $DIR"
    mkdir -p "${DIR}"
    pushd "${DIR}" > /dev/null

    if [ -e "$DL_DIR/$2.7z" ]; then
        echo "I/Copying archive found at: $DL_DIR/$2.7z"
        cp "$DL_DIR/$2.7z" "./$DLFILENAME"
    fi

    if [ $? != 0 ]; then
        echo "E/Can't cd to the toolset's directory '$DIR'."
        return 1;
    elif [ -e "$DLFILENAME" ]; then
        echo "I/Extracting $DLFILENAME"
        7z x -y "$DLFILENAME" | grep -v -e "\(7-Zip\|Processing\|Extracting\|^$\)" -
        if [ ${PIPESTATUS[0]} != 0 ]; then
            echo "E/Extraction failed ! Removing corrupted archive, please re-run the environment setup."
            rm "$DLFILENAME"
            popd
            return 1;
        fi
        popd
    elif [ ! -e "$DLFILENAME" ]; then
        echo "I/Trying download from ${ARCH_URL}"
        dl_file "$DLFILENAME.dlfile" "$ARCH_URL"
        if [ $? != 0 ]; then
            echo "E/Download failed !"
            popd
            return 1;
        fi
        mv "$DLFILENAME.dlfile" "$DLFILENAME"
        echo "I/Extracting $DLFILENAME"
        7z x -y "$DLFILENAME" | grep -v -e "\(7-Zip\|Processing\|Extracting\|^$\)" -
        if [ ${PIPESTATUS[0]} != 0 ]; then
            echo "E/Extraction failed ! Removing corrupted archive, please re-run the environment setup."
            rm "$DLFILENAME"
            popd
            return 1;
        fi
        popd
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
    echo "TOOLSETS = ${HAM_IMPORTED_TOOLSETS}"
    echo "MAIN TOOLSET = ${HAM_TOOLSET}, NAME: ${HAM_TOOLSET_NAME}, DIR: ${HAM_TOOLSET_DIR}"
    echo -n "TOOLS VERSION = "
    echo "$HAM_TOOLSET_VERSIONS"
    echo "==================================================="
}

toolset_import_list() {
    for ARG in $@
    do
        . ham-toolset-import.sh $ARG || return 1
        export HAM_IMPORTED_TOOLSET=$ARG
    done
}

toolset_is_imported() {
    ni-hget HAM_IMPORTS_TOOLSETS $1
}

toolset_check_imported() {
    for ARG in $@
    do
        if [[ -z "`toolset_is_imported "$ARG"`" ]]; then
            echo "E/'$ARG' toolset not imported."
            return 1
        fi
    done
}

toolset_bak_mv() {
    if [ -e "$1" ]; then
        mv -f "$1" "$1__bak"
    fi;
}

toolset_bak_rm() {
    if [ -e "$1__bak" ]; then
        rm -Rf "$1__bak"
    fi;
}

# usage: toolset_dl TOOLSET_NAME TOOLSET_DL_NAME
toolset_dl() {
    toolset_dl_and_extract $@
    toolset_dl_cleanup $@
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
        toolset_bak_mv ${TS_LOA}
        toolset_dl_and_extract ${TS_NAME} ${TS_VER_NAME}
        if [ ! -e "${TS_DIR}/${TS_VER_FILE_NAME}" ]; then
            echo "E/Toolset '$TS_NAME': Can't find '${TS_VER_FILE_NAME}' in '${TS_DIR}'."
            return 1
        fi
        toolset_dl_cleanup ${TS_NAME} ${TS_VER_NAME}
        toolset_bak_rm ${TS_LOA}
    else
        toolset_dl_cleanup ${TS_NAME} ${TS_VER_NAME}
    fi
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
  local dir;
  while true; do
    # exit with ^D
    dir="$(ls -a1F | grep '[/@]$' | grep -v '^./$' | sed 's/@$//' | fzf --height 40% --reverse --no-multi --preview 'pwd' --preview-window=up,1,border-none --no-info)"
    if [[ -z "${dir}" ]]; then
      break
    elif [[ -d "${dir}" ]]; then
      cd "${dir}"
    fi
  done
}

#
# fhh (print)
#
# Note: Can't be in an external script because each bash script as an
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
        fzf --expect=tab)
    KEY=`echo "${CMD}" | head -1`
    CMD=`echo "${CMD}" | tail -1`
    # echo "I/KEY: $KEY"
    # echo "I/COMMAND: $CMD"
    if [ "$1" == "print" ] || [ -n "$KEY" ]; then
      # echo "I/Print"
      echo "${CMD}"
    else
      # echo "I/Execute"
      history -s $CMD
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
    PREVTAG=`cat "$TAGFILE"`
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
  echo "$TAG" > "$OUTFILE"
  echo "I/Updated tagfile: '$OUTFILE'"
}

########################################################################
##  Environments
########################################################################
# Set HAM_OS first, its used by the script commands
if [[ $OS == Windows* ]]; then
    export HAM_OS=NT
    export HAM_BIN_LOA=nt-x86
    if [ -z "$HOME" ]; then
        export HOME=`unxpath "$USERPROFILE"`
    fi
elif [[ "$OSTYPE" == "darwin"* ]]; then
    export HAM_OS=OSX
    UNAME_STR=`uname`
    MACHINE_TYPE=`uname -m`
    if [ "$MACHINE_TYPE" == "x86_64" ]; then
        IS_TRANSLATED=`sysctl -n sysctl.proc_translated 2> /dev/null ; true`
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
    UNAME_STR=`/bin/uname`
    MACHINE_TYPE=`/bin/uname -m`
    if [ "$UNAME_STR" == "Linux" ] && [ "$MACHINE_TYPE" == "x86_64" ]; then
        export HAM_OS=LINUX
        export HAM_BIN_LOA=lin-x64
        export HAM_LOCAL_HOMEBREW="$HAM_HOME/toolsets/_brew/$HAM_BIN_LOA"
    else
        echo "W/Unknown OS '$UNAME_STR' '$MACHINE_TYPE'"
    fi
fi

if [[ -z $BUILD_BIN_LOA ]]; then
    export BUILD_BIN_LOA=$HAM_BIN_LOA
fi
if [[ -z $TEMPDIR ]]; then
    export TEMPDIR=$HOME/_ham
fi

if [[ -z "$WORK" ]]; then
    export WORK=`nativedir "$HAM_HOME/.."`
    if [ "$HAM_NO_VER_CHECK" != "1" ]; then
        echo "W/WORK not set, set to '$WORK' by default."
    fi
fi
export WORK=`unxpath "$WORK"`

# Detect the default number of jobs
if [[ -z $HAM_NUM_JOBS ]]; then
    if [[ -n $NUMBER_OF_PROCESSORS ]]; then
        export HAM_NUM_JOBS=$NUMBER_OF_PROCESSORS
    elif [[ -f /proc/cpuinfo ]]; then
        export HAM_NUM_JOBS=$(grep -c processor /proc/cpuinfo)
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        export HAM_NUM_JOBS=$(sysctl -n machdep.cpu.thread_count)
    else
        export HAM_NUM_JOBS=2
    fi
fi

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

# Make sure HAM_HOME has the proper unix format
export HAM_HOME=`unxpath "$HAM_HOME"`
