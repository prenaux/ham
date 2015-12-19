# This is solves problem with tools that really try hard to screw up the
# environment by reseting the path manually.
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
    case $HAM_OS in
        NT*)
            export PATH=$WORK/niSDK/bin:$HAM_HOME/bin:$HAM_HOME/bin/nt-x86:$BASH_START_PATH
            export PATH=$PATH:`unxpath $WINDIR`/System32
            ;;
        OSX)
            export PATH=$WORK/niSDK/bin:$HAM_HOME/bin:$HAM_HOME/bin/osx-x86:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:$BASH_START_PATH
            ;;
        LINUX)
            export PATH=$WORK/niSDK/bin:$HAM_HOME/bin:$HAM_HOME/bin/$HAM_BIN_LOA:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:$BASH_START_PATH
            ;;
        *)
            echo "W/ham-bash-setenv.sh: Unknown HAM_OS: $HAM_OS"
            ;;
    esac
    export PATH_BACKUP=$PATH
else
    true
fi

export HAM_ENV_SETUP=1
