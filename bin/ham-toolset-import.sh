########################################################################
toolset_import() {
    export PATH=$PATH
    . ham-toolset-import.sh $1
    if [ $? != 0 ]; then
        echo "E/Toolset '$1' import failed !"
        return 1
    else
        echo "I/Imported toolset '$1'."
    fi
}

toolset_dl() {
    export CWD=`pwd`
    export DIR=${HAM_HOME}/toolsets/$1
    export ARCH_URL1="http://localhost:8123/data/toolsets/$2.7z"
    export ARCH_URL2="https://bitbucket.org/prenaux/ham/downloads/$2.7z"
    export DLFILENAME="_$2.7z"
    cd ${DIR}
    if [ ! -e "$DLFILENAME" ]; then
        echo "... Trying download from ${ARCH_URL1}"
        wget -c --no-check-certificate $ARCH_URL1 -O"$DLFILENAME.wget"
        if [ $? != 0 ]; then
            echo "... Trying download from ${ARCH_URL2}"
            wget -c --no-check-certificate $ARCH_URL2 -O"$DLFILENAME.wget"
            if [ $? != 0 ]; then
                echo "Download failed !"
                return 1;
            fi
        fi
        mv "$DLFILENAME.wget" "$DLFILENAME"
    fi
    echo "... Extracting $DLFILENAME"
    7z x -y $DLFILENAME | grep -v -e "\(7-Zip\|Processing\|Extracting\|^$\)" -
    cd ${CWD}
}

export DIR=${HAM_HOME}/toolsets/$1
export SETUP_SCRIPT=$DIR/setup-toolset.sh
if [ ! -f "$SETUP_SCRIPT" ]; then
    echo "E/Can't find the toolset '$1'"
    return 1
fi

export PATH=$PATH
. $SETUP_SCRIPT
if [ $? != 0 ]; then
    return 1
fi
