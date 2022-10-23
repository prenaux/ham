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
    export PATH=$BASH_START_PATH
    case $HAM_OS in
        NT*)
            pathenv_remove_add "`unxpath $WINDIR`/System32" after
            pathenv_add "$HAM_HOME/bin/nt-x86"
            ;;
        OSX)
            pathenv_remove_add /opt/homebrew/bin after
            pathenv_remove_add /opt/homebrew/sbin after
            pathenv_remove_add /usr/local/bin after
            pathenv_remove_add /usr/bin after
            pathenv_remove_add /usr/sbin after
            pathenv_remove_add /bin after
            pathenv_remove_add /sbin after
            pathenv_remove_add /Library/Apple/usr/bin after
            pathenv_add "$("$HAM_HOME/bin/ham-brew-installdir" prefix)/bin"
            pathenv_add "$HAM_HOME/bin/osx"
            ;;
        LINUX)
            pathenv_remove_add /usr/local/sbin after
            pathenv_remove_add /usr/local/bin after
            pathenv_remove_add /usr/sbin after
            pathenv_remove_add /usr/bin after
            pathenv_remove_add /sbin after
            pathenv_remove_add /bin after
            pathenv_add "$("$HAM_HOME/bin/ham-brew-installdir" prefix)/bin"
            pathenv_add "$HAM_HOME/bin/linux"
            ;;
        *)
            echo "W/ham-bash-setenv.sh: Unknown HAM_OS: $HAM_OS"
            ;;
    esac
    pathenv_add "$HAM_HOME/bin/$HAM_BIN_LOA"
    # This must be called after bin/HAM_BIN_LOA so that they
    # get before it in the PATH list.
    pathenv_add "$HAM_HOME/bin"
    pathenv_add "$WORK/niLang/bin"
    export PATH_BACKUP=$PATH
else
    true
fi

export HAM_ENV_SETUP=1
