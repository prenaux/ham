#!/bin/bash
if [ -z "$HAM_HOME" ]; then
    echo "E/HAM_HOME not set !"
    return 1
fi
if [ -z "$WORK" ]; then
    echo "E/WORK not set !"
    return 1
fi
. "$HAM_HOME/bin/ham-bash-setenv.sh"

print_help() {
    echo "syntax:"
    echo "    . ham-pod-import.sh POD_NAME POD_LOA POD_VER"
    echo "example:"
    echo "    . ham-pod-import.sh niSDK nt-x86 v1"
}

. "$HAM_HOME/bin/ham-pod-setenv.sh" $1 $2 $3
if [ $? != 0 ]; then
    print_help
    return 1
fi

if [ ! -e "${POD_DIR}/${POD_VER_FILE_NAME}" ]; then
    echo "E/Pod '$POD_NAME': Can't find '${POD_VER_FILE_NAME}' in '${POD_DIR}'."
    print_help
    return 1
fi

ALREADY_IMPORTED=`ni-hget HAM_IMPORTS_POD ${POD_NAME}`
if [[ $ALREADY_IMPORTED = "1" ]]; then
    echo "I/Already imported pod '$POD_NAME'."
else
    export PATH=$PATH
    export POD_VAR_NAME=${POD_NAME}_${POD_LOA/-/_}_${POD_VER}
    eval export "HAM_POD_DIR_${POD_NAME}"=\"`nativedir "${POD_DIR}"`\"
    eval export "HAM_POD_LOA_${POD_NAME}"="${POD_LOA}"
    eval export "HAM_POD_VER_${POD_NAME}"="${POD_VER}"
    if [ -e "${POD_DIR}/_ham_pod" ]; then
        source "${POD_DIR}/_ham_pod"
    fi
    if [ -e "${POD_DIR}/bin" ]; then
        export PATH=${POD_DIR}/bin:$PATH
    fi
    if [ -e "${POD_DIR}/bin/${POD_LOA}" ]; then
        export PATH=${POD_DIR}/bin/${POD_LOA}:$PATH
    fi

    if [ $? != 0 ]; then
        echo "E/Pod '$POD_NAME' import failed !"
        return 1
    else
        if [[ -z $HAM_IMPORTED_PODS ]]; then
            export HAM_IMPORTED_PODS="$POD_VAR_NAME"
        else
            export HAM_IMPORTED_PODS="$HAM_IMPORTED_PODS $POD_VAR_NAME"
        fi
        ni-hput HAM_IMPORTS_POD ${POD_VAR_NAME} 1
        echo -e "I/Imported pod '$POD_VAR_NAME'."
    fi
fi
