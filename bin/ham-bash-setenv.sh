if [[ -z $HAM_HOME ]]; then
    echo "E/HAM_HOME not set !"
    exit 1
fi
. $HAM_HOME/bin/ham-bash-lib.sh

########################################################################
##  HAM_OS
########################################################################
if [[ $OS == Windows* ]]; then
    export HAM_OS=NT
    export HAM_BIN_LOA=nt-x86
else
    echo "W/Unknown OS"
    # exit 1
fi

########################################################################
##  PATH
########################################################################
export PATH=$HAM_HOME/bin:$HAM_HOME/bin/nt-x86
