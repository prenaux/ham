if [[ -z $HAM_HOME ]]; then
    echo "E/HAM_HOME not set !"
    exit 1
fi

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
    DIRPATH=$1
    case $HAM_OS in
        NT*)
            2>/dev/null cd "$DIRPATH"; pwd -W
            ;;
        *)
            echo $DIRPATH
            ;;
    esac
}

unxpath()
# usage: NativePathNameVariable=`unx32path "/MSYS/PathName"`
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

thisscriptdir() {
    DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
    echo `unxpath $DIR`
}

update_prompt() {
    export PS1='
\[\033[35m$HAM_TOOLSET_NAME\033[0m\] \w (\[\033[32m$USERNAME\033[0m\])
$ '
	# echo -e "\033]0;`pwd`\007"
    # export PS1="${AGL_TOOLSET_NAME}\$ "
}

upsearch() {
    test / == "$PWD" && return || test -e "$1" && echo "$PWD" && return || cd .. && upsearch "$1"
}

########################################################################
##  Environments
########################################################################
# Set HAM_OS first, its used by the script commands
if [[ $OS == Windows* ]]; then
    export HAM_OS=NT
    export HAM_BIN_LOA=nt-x86
    if [ -z $HOME ]; then
        export HOME=`unxpath $USERPROFILE`
    fi
else
    echo "W/Unknown OS"
    # exit 1
fi

if [[ -z $WORK ]]; then
    export WORK=`nativedir $HAM_HOME/..`
    export WORK=`unxpath $WORK`
    echo "W/WORK not set, set to '$WORK' by default."
fi

if [[ -z $HOME ]]; then
    echo "E/HOME not set !"
    exit 1
fi

# Make sure HAM_HOME has the proper unix format
export HAM_HOME=`unxpath $HAM_HOME`
