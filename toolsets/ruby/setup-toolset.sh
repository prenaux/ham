#!/bin/bash

# toolset
export HAM_TOOLSET=RUBY
export HAM_TOOLSET_VER=221
export HAM_TOOLSET_NAME=ruby
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/ruby"

# path setup
case $HAM_OS in
    OSX*)
        export RUBY_DIR="${HAM_TOOLSET_DIR}/osx-x86/"
        export PATH="${RUBY_DIR}/bin":${PATH}
        if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
        ;;
    NT*)
        export RUBY_DIR="${HAM_TOOLSET_DIR}/nt-x86/"
        export PATH="${RUBY_DIR}/bin":${PATH}
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

VER="--- ruby ------------------------
`ruby --version`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
