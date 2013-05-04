if [[ -z $HAM_HOME ]]; then
    echo "E/HAM_HOME not set !"
    exit 1
fi

if [[ `pwd` == "/usr" ]]; then
    cd $HAM_HOME/..
fi

export BASH_START_PATH=$PATH
. $HAM_HOME/bin/ham-bash-lib.sh
. $HAM_HOME/bin/ham-bash-setenv.sh

echo "=== Ham bash shell ==="
echo "HAM_HOME = $HAM_HOME"
echo "HAM_OS = $HAM_OS"
echo "HAM_BIN_LOA = $HAM_BIN_LOA"

update_prompt
