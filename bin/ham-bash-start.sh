if [[ -z $HAM_HOME ]]; then
    echo "E/HAM_HOME not set !"
    exit 1
fi

if [[ `pwd` == "/usr" ]]; then
    cd "$HAM_HOME/.."
fi

export BASH_START_PATH=$PATH
. "$HAM_HOME/bin/ham-bash-setenv.sh"

if [[ "$BASH_START_SILENT" = "" ]]; then
    echo "=== Ham bash shell ==="
    echo "WORK = $WORK"
    echo "HAM_HOME = $HAM_HOME"
    echo "HAM_OS = $HAM_OS"
    echo "HAM_BIN_LOA = $HAM_BIN_LOA"
fi

update_prompt
