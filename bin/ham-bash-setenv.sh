. $HAM_HOME/bin/ham-bash-lib.sh

########################################################################
##  PATH
########################################################################
case $HAM_OS in
    NT*)
        export PATH=$HAM_HOME/bin:$HAM_HOME/bin/nt-x86:$BASH_START_PATH
        export PATH=$PATH:"`unxpath $WINDIR`/System32"
        ;;
    OSX)
        export PATH=$HAM_HOME/bin:$HAM_HOME/bin/osx-x86:/usr/bin:/bin:/usr/sbin:/sbin:$BASH_START_PATH
        ;;
esac
