#!/bin/bash -e
#===== PRELUDE BEGIN ===========
if [[ -z "$HAM_HOME" ]]; then
  echo "E/HAM_HOME not set !"
  exit 1
fi
# shellcheck disable=SC2034
SCRIPT_NAME=$(basename "$0")
# shellcheck disable=SC2034
SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
. "$HAM_HOME/bin/ham-bash-setenv.sh"
#===== PRELUDE END =============

# the help
usage() {
  echo "usage:"
  echo "  ham-svn.sh checkout"
  echo "  ham-svn.sh ls_local DIRNAME"
  echo "  ham-svn.sh ls_remote DIRNAME"
  echo "  ham-svn.sh revertdir DIRNAME"
  echo "  ham-svn.sh adddir_r DIRNAME"
  echo "  ham-svn.sh adddir_f DIRNAME"
  echo "  ham-svn.sh adddir_only DIRNAME"
  echo "  ham-svn.sh mkdir DIRNAME"
  echo "  ham-svn.sh ignore_update DIRNAME"
  echo "  ham-svn.sh commit_message DIRNAME"
  echo "  ham-svn.sh log DIRNAME"
  echo "  ham-svn.sh logv DIRNAME"
  echo "  ham-svn.sh status DIRNAME"
  echo "  ham-svn.sh update DIRNAME"
  echo "  ham-svn.sh commit (yes) DIRNAME (COMMIT_MESSAGE)"
  echo ""

  if [[ "${BASH_SOURCE[0]}" != "${0}" ]]; then
    # sourced...
    return 1
  else
    # regular call
    exit 1
  fi
}

if [ -z "$HAM_SVN_URL" ]; then
  log_error "HAM_SVN_URL not set."
  usage
fi
if [ -z "$HAM_SVN_LOCAL" ]; then
  log_error "HAM_SVN_LOCAL not set."
  usage
fi

if [ ! -d "$HAM_SVN_LOCAL" ]; then
  log_error "HAM_SVN_LOCAL '$HAM_SVN_LOCAL' doesn't exist."
  usage
fi

COMMAND=$1
if [ -z "$COMMAND" ]; then
  log_error "command not specified"
  usage
fi
shift

function generate_commit_message() {
  cd "$1"
  modified_dirs=$(svn status . | grep -v "^?" | awk '{print $2}' | xargs -I{} dirname {} | coreutils sort | coreutils uniq | coreutils paste -sd "," -)
  echo "Updated $(basename "$HAM_SVN_URL") on $(arch_date) in $modified_dirs"
}

case $COMMAND in
  checkout)
    (
      set -x
      svn checkout "${HAM_SVN_URL}/" "${HAM_SVN_LOCAL}"
    )
    ;;

  ls_local)
    DIRNAME="$1"
    if [ -z "$DIRNAME" ]; then
      log_error "DIRNAME not specified."
      usage
    fi
    shift

    log_info "Listing folder..."
    (
      set -ex
      ls -lah "$HAM_SVN_LOCAL/$DIRNAME"
    )
    ;;

  ls_remote)
    DIRNAME="$1"
    if [ -z "$DIRNAME" ]; then
      log_error "DIRNAME not specified."
      usage
    fi
    shift

    log_info "Listing remote..."
    (
      set -ex
      svn ls "$HAM_SVN_URL/$DIRNAME"
    )
    ;;

  revertdir)
    DIRNAME="$1"
    if [ -z "$DIRNAME" ]; then
      log_error "DIRNAME not specified."
      usage
    fi
    shift
    (
      set -x
      svn revert --depth infinity "$HAM_SVN_LOCAL/$DIRNAME"
    )
    ;;

  adddir_r)
    DIRNAME="$1"
    if [ -z "$DIRNAME" ]; then
      log_error "DIRNAME name not specified."
      usage
    fi
    shift
    (
      set -x
      svn add --parents --depth=infinity --force "$HAM_SVN_LOCAL/$DIRNAME"
    )
    ;;

  adddir_f)
    DIRNAME="$1"
    if [ -z "$DIRNAME" ]; then
      log_error "DIRNAME not specified."
      usage
    fi
    shift
    (
      set -x
      svn add --parents --depth=files --force "$HAM_SVN_LOCAL/$DIRNAME"
    )
    ;;

  mkdir)
    DIRNAME="$1"
    if [ -z "$DIRNAME" ]; then
      log_error "DIRNAME name not specified."
      usage
    fi
    shift

    (
      set -x
      svn mkdir "$HAM_SVN_URL/$DIRNAME" -m "Mkdir '$DIRNAME' in '$(basename "$HAM_SVN_URL")' on $(arch_date)."
    )
    ;;

  adddir_only)
    DIRNAME="$1"
    if [ -z "$DIRNAME" ]; then
      log_error "DIRNAME not specified."
      usage
    fi
    shift
    (
      set -x
      svn add --parents --depth=empty --force "$HAM_SVN_LOCAL/$DIRNAME"
    )
    ;;

  ignore_update)
    DIRNAME="$1"
    if [ -z "$DIRNAME" ]; then
      log_error "DIRNAME not specified."
      usage
    fi
    shift
    (
      set -x
      svn-ignore-update "$HAM_SVN_LOCAL/$DIRNAME"
    )
    ;;

  commit_message)
    DIRNAME="$1"
    if [ -z "$DIRNAME" ]; then
      log_error "DIRNAME not specified."
      usage
    fi
    shift
    generate_commit_message "$HAM_SVN_LOCAL/$DIRNAME"
    ;;

  log)
    DIRNAME="$1"
    if [ -z "$DIRNAME" ]; then
      log_error "DIRNAME not specified."
      usage
    fi
    shift
    (
      set -x
      svn log -l 3 "$HAM_SVN_URL/$DIRNAME"
    )
    ;;

  logv)
    DIRNAME="$1"
    if [ -z "$DIRNAME" ]; then
      log_error "DIRNAME not specified."
      usage
    fi
    shift
    (
      set -x
      svn log -v -l 3 "$HAM_SVN_URL/$DIRNAME"
    )
    ;;

  check_status)
    DIRNAME="$1"
    if [ -z "$DIRNAME" ]; then
      log_error "DIRNAME not specified."
      usage
    fi
    shift
    cd "$HAM_SVN_LOCAL/$DIRNAME"
    svn status .
    ;;

  status)
    DIRNAME="$1"
    if [ -z "$DIRNAME" ]; then
      log_error "DIRNAME not specified."
      usage
    fi
    shift
    (
      set -x
      cd "$HAM_SVN_LOCAL/$DIRNAME"
      svn status .
    )
    ;;

  update)
    DIRNAME="$1"
    if [ -z "$DIRNAME" ]; then
      log_error "DIRNAME not specified."
      usage
    fi
    shift
    (
      set -x
      svn update "$HAM_SVN_LOCAL/$DIRNAME"
    )
    ;;

  commit)
    CHOICE=no
    if [[ "$1" == "yes" ]]; then
      CHOICE=yes
      shift
    fi

    DIRNAME="$1"
    if [ -z "$DIRNAME" ]; then
      log_error "DIRNAME not specified."
      usage
    fi
    shift

    COMMIT_MESSAGE="$1"
    if [ -z "$COMMIT_MESSAGE" ]; then
      COMMIT_MESSAGE=$(generate_commit_message "$HAM_SVN_LOCAL/$DIRNAME")
    else
      shift
    fi

    log_info "This is what will be commited:"
    (
      set -x
      cd "$HAM_SVN_LOCAL/$DIRNAME"
      svn status -q .
    )

    log_info "The commit message will be: $COMMIT_MESSAGE"
    if [ "$CHOICE" != "yes" ]; then
      read -r -p "Continue (y/N)? " CHOICE
    fi
    case "$CHOICE" in
      y | Y | yes)
        log_info "Comitting..."
        (
          set -x
          cd "$HAM_SVN_LOCAL/$DIRNAME"
          svn commit -m "$COMMIT_MESSAGE"
        )
        ;;
      *)
        echo "W/Skipped commit."
        ;;
    esac
    ;;

  *)
    log_error "Unknown command '$COMMAND'"
    usage
    ;;
esac
