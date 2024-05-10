#!/bin/bash
toolset_import_once repos || return 1
toolset_import_once python_3 || return 1

# toolset
export HAM_TOOLSET=AWS
export HAM_TOOLSET_NAME=aws
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/${HAM_TOOLSET_NAME}"

# path setup
case $HAM_OS in
  NT)
    if [ ! -e "$PYTHON3_BINDIR/eb" ]; then
      echo "I/eb not found, installing..."
      ham-pip3 install awsebcli --upgrade --user
      errcheck $? aws "E/Can't pip3 install awsebcli."
    fi
    if [ ! -e "$PYTHON3_BINDIR/aws" ]; then
      echo "I/aws not found, installing..."
      ham-pip3 install awscli --upgrade --user
      errcheck $? aws "E/Can't pip3 install awscli."
    fi
    ;;
  OSX)
    ham-brew-install awscli "bin/aws" || return 1
    ham-brew-install aws-elasticbeanstalk "bin/eb" || return 1
    ;;
  LINUX)
    if [ ! -x "$(command -v eb)" ]; then
      echo "I/eb not found, installing..."
      ham-pip3 install awsebcli --upgrade --user
    fi
    if [ ! -x "$(command -v aws)" ]; then
      echo "I/aws not found, installing..."
      ham-pip3 install awscli --upgrade --user
    fi
    ;;
  *)
    echo "E/Toolset: Unsupported host OS"
    return 1
    ;;
esac

# path
pathenv_add "$HAM_TOOLSET_DIR"

VER="--- aws ---------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  if ! VER="$VER
$(aws --version)"; then
    echo "E/Can't get aws-cli version."
    return 1
  fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"

VER="--- aws-eb ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  if ! VER="$VER
$(aws-eb --version)"; then
    echo "E/Can't get aws-eb version."
    return 1
  fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
