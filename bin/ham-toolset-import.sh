export DIR=${HAM_HOME}/toolsets/$1
export SETUP_SCRIPT=$DIR/setup-toolset.sh
if [ ! -f "$SETUP_SCRIPT" ]; then
    echo "E/Can't find the toolset '$1'"
    return 1
fi

ALREADY_IMPORTED=`ni-hget HAM_IMPORTS $1`

if [[ $ALREADY_IMPORTED = "1" ]]; then
    echo "I/Already imported '$1'."
else
    export PATH=$PATH
    . $SETUP_SCRIPT
    if [ $? != 0 ]; then
        echo "E/Toolset '$1' import failed !"
        return 1
    else
        ni-hput HAM_IMPORTS $1 1
        echo "I/Imported toolset '$1'."
    fi
fi
