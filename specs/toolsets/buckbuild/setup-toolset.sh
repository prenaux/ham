#!/bin/bash
toolset_import_once python_3 || return 1

export HAM_TOOLSET=BUCKBUILD
export HAM_TOOLSET_NAME=buckbuild
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/buckbuild"

# dist
toolset_check_and_dl_ver buckbuild dist v20210505 || return 1
export BUCKBUILD_DIST_DIR="${HAM_TOOLSET_DIR}/dist"

# watchman
case $HAM_OS in
    OSX*)
        ham-brew-install watchman "bin/watchman" || return 1
        ;;
    *)
        echo "W/Toolset: watchman not supported on ${HAM_OS}"
        ;;
esac

# path
pathenv_add "${HAM_TOOLSET_DIR}"

# version
VER="--- buckbuild ----------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
    VER="$VER
`ham-buck --version`"
    if [ $? != 0 ]; then
        echo "E/Can't get buckbuild version."
        return 1
    fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
