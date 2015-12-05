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

else
    true
fi

export HAM_ENV_SETUP=1
