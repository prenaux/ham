. $HAM_HOME/bin/ham-bash-lib.sh

########################################################################
##  PATH
########################################################################
export PATH=$HAM_HOME/bin:$HAM_HOME/bin/nt-x86:$BASH_START_PATH
case $HAM_OS in
    NT*)
        export PATH=$PATH:"`unxpath $WINDIR`/System32"
        ;;
esac
