#!/bin/bash
toolset_import_once repos || return 1

# toolset
export HAM_TOOLSET=SVN
export HAM_TOOLSET_NAME=svn
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/svn"

# platform
case $HAM_OS in
  NT*)
    toolset_check_and_dl_ver svn nt-x86 v1_14_1 || return 1
    pathenv_add "${HAM_TOOLSET_DIR}/nt-x86/bin"
    # Mercifully msys git has git svn bundled in by default, so the repos toolset is enough.
    ;;
  OSX*)
    ham-brew-install svn "bin/svn"
    ham-brew-install git-svn "bin/git-svn"
    ;;
  LINUX*)
    if [ -z "$(which svn)" ]; then
      ham-apt-get-install subversion
    fi
    if [[ ! -e "$(git --exec-path)/git-svn" ]]; then
      # This is GITHUB_ACTION workaround because they seem to have
      # bypassed regular install to coerce a more recent git version which
      # result in the git-svn dependency check failing.
      # 1) We install the actual dependencies
      ham-apt-get-install git libsvn-perl libyaml-perl libterm-readkey-perl
      # 2) Install git-svn without checking any dependencies AND we have
      #    to force the dl URL because even that is custom in the github
      #    action
      lin-apt-install-nodeps git-svn "http://archive.ubuntu.com/ubuntu/pool/universe/g/git/git-svn_2.34.1-1ubuntu1.10_all.deb"
    fi
    ;;
  *)
    echo "E/Toolset: Unsupported host OS"
    return 1
    ;;
esac

pathenv_add "${HAM_TOOLSET_DIR}"

# version check
if ! VER="--- svn --------------------------
$(svn --version | grep ", version")"; then
  echo "E/Can't get SVN version."
  return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"

if ! VER="--- git-svn ----------------------
$(git svn --version)"; then
  echo "E/Can't get SVN version."
  return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
