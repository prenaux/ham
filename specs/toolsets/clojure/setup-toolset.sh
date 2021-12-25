#!/bin/bash

# toolset
export HAM_TOOLSET=clojure
export HAM_TOOLSET_NAME=clojure
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/clojure"

# path setup
case $HAM_OS in
    OSX*)
        ham-brew-install "clojure/tools/clojure" "bin/clojure"
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

VER="--- clojure --------------------
`clojure --version`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
