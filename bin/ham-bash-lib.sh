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

# Serious BS from Cygwin...
export CYGWIN=nodosfilewarning

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
# usage: complain ModuleName "Diagnostic Message"
#
# Display a message of the form `aglDevEnv.ModuleName: Diagnostic Message',
# then continue aglDevEnv execution.
{
  echo >&2 "E/$1: $2"
}

die()
# usage: die ModuleName "Message Saying Why"
#
# Display a diagnostic message, and cause the aglDevEnv to abort; this
# is the function dispatcher invoked by `require', when the specified
# "aglDevEnv.ModuleName" file cannot be sourced; it may also be invoked
# directly from any sourced "aglDevEnv.ModuleName" file, to diagnose
# any fatal condition.
{
  complain "$@"
  exit 1
}

errcheck()
{
	if [ $1 != 0 ]
	then
		die $2 "$3 (errcode $1)"
	fi
}

nativedir()
# usage: NativePathNameVariable=`nativedir "/MSYS/PathName"`
{
    DIRPATH="$1"
    case $HAM_OS in
        NT*)
            2>/dev/null cd "$DIRPATH"; pwd -W
            ;;
        *)
            # echo "$DIRPATH"
            echo $( cd "$DIRPATH" && pwd )
            ;;
    esac
}

unxpath()
# usage: NativePathNameVariable=`unxpath "/MSYS/PathName"`
#
# Determine the native Native path name equivalent for the POSIX style
# "/MSYS/PathName";  (CAVEAT:  the specified "/MSYS/PathName" *must*
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

thisscriptdir() {
    DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
    echo `unxpath "$DIR"`
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

    export PS1='
\[\033[35m$PROJECT_NAME$TOOLSET_EXTRA\033[0m\] \[\033[32m$(current_git_branch)\033[0m\]\w (\[\033[32m$USERNAME\033[0m\]) \[\033[0;36m$HAM_IMPORTED_TOOLSETS\033[0m\]
$ '
	# echo -e "\033]0;`pwd`\007"
  # export PS1="${AGL_TOOLSET_NAME}\$ "
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
        echo "I/Already imported '$1'."
    else
        . ham-toolset-import.sh $1
    fi
}

toolset_dl_and_extract() {
    export CWD=`pwd`
    export DIR="${HAM_HOME}/toolsets/$1"
    # export ARCH_URL="http://localhost:8123/data/toolsets/$2.7z"
    export ARCH_URL="http://cdn2.talansoft.com/toolsets/$2.7z"
    export DLFILENAME="_$2.7z"
    echo "=== Importing toolset '$1' from $DIR"
    mkdir -p "${DIR}"
    pushd "${DIR}" > /dev/null
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
        wget -c --no-check-certificate $ARCH_URL -O"$DLFILENAME.wget"
        if [ $? != 0 ]; then
            echo "E/Download failed !"
            popd
            return 1;
        fi
        mv "$DLFILENAME.wget" "$DLFILENAME"
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
    echo "PODS = ${HAM_IMPORTED_PODS}"
    echo "TOOLSETS = ${HAM_IMPORTED_TOOLSETS}"
    echo "MAIN TOOLSET = ${HAM_TOOLSET}, VER: ${HAM_TOOLSET_VER}, NAME: ${HAM_TOOLSET_NAME}, DIR: ${HAM_TOOLSET_DIR}"
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
##  Pods
########################################################################
# usage: pod_import NAME LOA VERSION
pod_import() {
    ALREADY_IMPORTED=`ni-hget HAM_IMPORTS_PODS $1`
    if [[ $ALREADY_IMPORTED = "1" ]]; then
        echo "I/Already imported '$1'."
    else
        pod_check_and_dl_ver $1 $2 $3
        if [ $? != 0 ]; then
            return 1
        fi
        . ham-pod-import.sh $1 $2 $3
        if [ $? != 0 ]; then
            return 1
        fi
    fi
}

# usage: pod_check_and_dl_ver NAME LOA VERSION
# example: pod_check_and_dl_ver repos nt-x86 v2
pod_check_and_dl_ver() {
    . ham-pod-setenv.sh $1 $2 $3
    if [ ! -e "${POD_DIR}/${POD_VER_FILE_NAME}" ]; then
        pod_bak_mv ${POD_LOA}
        pod_dl_and_extract ${POD_NAME} ${POD_VER_NAME}
        if [ ! -e "${POD_DIR}/${POD_VER_FILE_NAME}" ]; then
            echo "E/Pod '$POD_NAME': Can't find '${POD_VER_FILE_NAME}' in '${POD_DIR}'."
            return 1
        fi
        pod_bak_rm ${POD_LOA}
    fi
}

# usage: pod_dl_and_extract name loa version
pod_dl_and_extract() {
    . ham-pod-setenv.sh $1 $2 $3
    export CWD=`pwd`
    export DLDIR="`nativedir "${HAM_HOME}"`/pods/_dl"
    export DLFILENAME="${POD_VER_NAME}.7z"
    # export ARCH_URL="http://cdn2.talansoft.com/pods/${DLFILENAME}"
    export ARCH_URL="https://tsdata2.blob.core.windows.net/pods/${DLFILENAME}"

    if [ ! -e "${DLDIR}/${DLFILENAME}" ]; then
        mkdir -p "${DLDIR}"
        pushd "${DLDIR}" > /dev/null
        if [ $? != 0 ]; then
            echo "E/Can't cd to the pod's download directory '$DLDIR'."
            return 1;
        fi
        echo "I/Downloading from '${ARCH_URL}' to '${DLDIR}/${DLFILENAME}'."
        wget -c --no-check-certificate "${ARCH_URL}" -O"${DLFILENAME}.wget"
        if [ $? != 0 ]; then
            echo "E/Download failed !"
            popd
            return 1;
        fi
        mv "$DLFILENAME.wget" "$DLFILENAME"
        popd
    fi

    if [ -e "${DLDIR}/${DLFILENAME}" ]; then
        mkdir -p "${POD_DIR}"
        pushd "${POD_DIR}" > /dev/null
        if [ $? != 0 ]; then
            echo "E/Can't cd to the pod's directory '$POD_DIR'."
            return 1;
        fi
        echo "I/Extracting '${DLDIR}/${DLFILENAME}'"
        7z x -y "${DLDIR}/${DLFILENAME}" | grep -v -e "\(7-Zip\|Processing\|Extracting\|^$\)" -
        if [ ${PIPESTATUS[0]} != 0 ]; then
            echo "E/Extraction failed ! Removing corrupted archive, please re-run the environment setup."
            rm "$DLFILENAME"
            popd
            return 1;
        fi
        popd
    fi
}

pod_bak_mv() {
    if [ -e "$1" ]; then
        mv -f "$1" "$1__bak"
    fi;
}

pod_bak_rm() {
    if [ -e "$1__bak" ]; then
        rm -Rf "$1__bak"
    fi;
}

# usage: pod_build NAME LOA VERSION
# Build a pod's package
pod_build() {
    echo "I/Building Pod '$1' $2 $3"
    . ham-pod-setenv.sh $1 $2 $3
    rm -f _ham_pod_$1
    echo $POD_VER_NAME > $POD_VER_FILE_NAME
    export DLFILENAME="${HAM_HOME}/pods/_dl/${POD_VER_NAME}.7z"
    if [ -e "${DLFILENAME}" ]; then
        mv "${DLFILENAME}" "${DLFILENAME}.bak"
    fi
    export BUILD_NIP="./_build_pod_${POD_LOA}.nip"
    if [ ! -e "${BUILD_NIP}" ]; then
        export BUILD_NIP="../_build_pod_${POD_LOA}.nip"
        if [ ! -e "${BUILD_NIP}" ]; then
            echo "Can't find pod build script '_build_pod_${POD_LOA}.nip'"
            return 1
        fi
    fi
    "$WORK/niSDK/bin/ni" -e -I "$WORK/niSDK/scripts" -I "$WORK/ham/scripts" "${BUILD_NIP}"
    if [ $? != 0 ]; then echo "Can't build $1 $2 pod."; return 1; fi
}

# usage: pod_up NAME LOA VERSION
# Upload a built pod package to the server
pod_up() {
    . ham-pod-setenv.sh $1 $2 $3
    export DLFILENAME="${HAM_HOME}/pods/_dl/${POD_VER_NAME}.7z"
    if [ ! -e "${DLFILENAME}" ]; then
        echo "E/Pod '$POD_NAME': Can't find ${DLFILENAME} to upload."
        return 1
    fi
    echo "I/Pod '$POD_NAME' found '${DLFILENAME}' to upload."
    cloud_copy "${DLFILENAME}" pods
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
    date -u +"%Y%m%d_%H%M%SZ"
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
    export HAM_BIN_LOA=osx-x64
else
    UNAME_STR=`/bin/uname`
    MACHINE_TYPE=`/bin/uname -m`
    if [ "$UNAME_STR" == "Linux" ] && [ "$MACHINE_TYPE" == "x86_64" ]; then
        export HAM_OS=LINUX
        export HAM_BIN_LOA=lin-x64
    else
        echo "W/Unknown OS '$UNAME_STR' '$MACHINE_TYPE'"
    fi
    # exit 1
fi

if [[ -z $BUILD_BIN_LOA ]]; then
    export BUILD_BIN_LOA=$HAM_BIN_LOA
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
