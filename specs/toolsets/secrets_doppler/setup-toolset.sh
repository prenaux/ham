#!/bin/bash

# toolset
export HAM_TOOLSET=secrets_doppler
export HAM_TOOLSET_NAME=secrets_doppler
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/secrets_doppler"

# path setup
case $HAM_OS in
    OSX*)
        ham-brew-install gnupg "bin/gpg" || return 1
        if [ -z "`which doppler`" ]; then
            (set -ex; curl -Ls --tlsv1.2 --proto "=https" --retry 3 https://cli.doppler.com/install.sh | sudo sh)
            if [ -z "`which doppler`" ]; then
                echo "E/Can't install doppler."
                return 1
            fi
        fi
        ;;
    LINUX*)
        DOPPLER_DIR=${HAM_TOOLSET_DIR}/$HAM_BIN_LOA
        if [ ! -f "${DOPPLER_DIR}/usr/bin/doppler" ]; then
            echo "W/doppler not found, installing from deb package..."
            mkdir -p "$DOPPLER_DIR"
            echo "I/Downloading doppler .deb package"
            curl --progress-bar -L --tlsv1.2 --proto "=https" --retry 3 https://github.com/DopplerHQ/cli/releases/download/3.38.0/doppler_3.38.0_linux_amd64.deb -O doppler_3.38.0_linux_amd64.deb
            echo "I/Unpacking deb package locally"
            # Assumes dpkg is available
            dpkg -x doppler_3.38.0_linux_amd64.deb ${DOPPLER_DIR}
            rm doppler_3.38.0_linux_amd64.deb
            if [ ! -f "${DOPPLER_DIR}/usr/bin/doppler" ]; then
                echo "E/Can't install doppler."
                return 1
            fi
        fi
        # Add to $PATH ensuring they are no duplicate entries
        pathenv_add "${DOPPLER_DIR}/usr/bin"
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

VER="--- secrets_doppler ------------
`doppler --version`"
if [ $? != 0 ]; then
    echo "E/Can't get Doppler version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
