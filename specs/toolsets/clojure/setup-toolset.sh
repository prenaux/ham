#!/bin/bash

# toolset
export HAM_TOOLSET=clojure
export HAM_TOOLSET_VER=1
export HAM_TOOLSET_NAME=clojure
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/clojure"

# path setup
case $HAM_OS in
    OSX*)
        export CLJ_HOME="`brew --prefix clojure/tools/clojure`";
        if [ ! -e "$CLJ_HOME/bin/clojure" ]; then
            echo "I/Brew clojure not found, trying to install."
            ham-brew install clojure/tools/clojure
            if [ ! -e "$CLJ_HOME/bin/clojure" ]; then
                echo "I/Brew clojure install failed."
                return 1
            fi
        fi
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
