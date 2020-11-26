if [ -z "$HAM_HOME" ]; then
    echo "E/HAM_HOME not specified."
    return 1
fi
if [ -z "$1" ]; then
    echo "E/POD_NAME not specified."
    return 1
fi
if [ -z "$2" ]; then
    echo "E/POD_LOA not specified."
    return 1
fi
if [ -z "$3" ]; then
    echo "E/POD_VER not specified."
    return 1
fi
export POD_NAME=$1
export POD_LOA=$2
export POD_VER=$3
export POD_DIR="${HAM_HOME}/pods/${POD_NAME}-${POD_VER}/${POD_LOA}"
export POD_VER_NAME=${POD_NAME}_${POD_LOA}_${POD_VER}
export POD_VER_FILE_NAME=_ham_pod_${POD_VER_NAME}
