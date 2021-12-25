#!/bin/bash

# toolset
export HAM_TOOLSET=RUBY
export HAM_TOOLSET_NAME=ruby
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/ruby"

# path setup
case $HAM_OS in
    OSX*)
        export RUBY_DIR="${HAM_TOOLSET_DIR}/osx-x86/"
        export PATH="${RUBY_DIR}/bin":"${HAM_TOOLSET_DIR}":${PATH}
        if which rbenv > /dev/null; then
            eval "$(rbenv init -)";
        else
            export RUBY_VERSION=2.2.1
            export PATH=$HOME/.rvm/gems/ruby-$RUBY_VERSION/bin:$HOME/.rvm/gems/ruby-$RUBY_VERSION@global/bin:$HOME/.rvm/rubies/ruby-$RUBY_VERSION/bin:$HOME/.rvm/bin:$PATH
            [[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"
        fi
        ;;
    NT*)
        export RUBY_DIR="${HAM_TOOLSET_DIR}/nt-x86/"
        export PATH="${RUBY_DIR}/bin":"${HAM_TOOLSET_DIR}":${PATH}
        if [ ! -e "$RUBY_DIR/bin" ]; then
            toolset_dl ruby ruby_nt-x86
            if [ ! -e "$RUBY_DIR/bin" ]; then
                echo "E/nt-x86 folder doesn't exist in the toolset"
                return 1
            fi
        fi
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
