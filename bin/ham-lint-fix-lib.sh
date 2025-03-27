#!/bin/bash
export HAM_LINT_ALL_FILENAME="_lint_all_files.txt"

function sh_lint() {
  toolset_import_once shell_linter >/dev/null
  errcheck $? "$SCRIPT_NAME" "Can't import shell_linter toolset." || return 1

  local FILES=()
  if [ -z "$1" ]; then
    # List the shell files in a folder
    while IFS='' read -r line; do
      FILES+=("$line")
    done < <("$HAM_SHELL_LINTER_DIR/shfmt" -f .)
  else
    FILES=("$@")
  fi

  if [[ "$LINT_FORMAT" == "yes" ]] || [[ "$LINT_CHECK_FORMAT" == "yes" ]]; then
    local SHFMT_PARAMS=(-i 2 -ci -bn -ln=bash -bn=false)
    if [[ "$LINT_FORMAT" == "yes" ]]; then
      SHFMT_PARAMS=(-w "${SHFMT_PARAMS[@]}")
    else
      SHFMT_PARAMS=(-d "${SHFMT_PARAMS[@]}")
    fi
    (
      set -x
      "$HAM_SHELL_LINTER_DIR/shfmt" "${SHFMT_PARAMS[@]}" "${FILES[@]}"
    )
  fi

  if [[ "$NO_LINT" != "yes" ]]; then
    local SHELLCHECK_PARAMS=(--shell=bash)

    SHELLCHECK_PARAMS=(-e SC1091) # 'Not following: ... was not specified as input'
    # This is better than disabling SC1091, but oh so slowww :(
    # SHELLCHECK_PARAMS=(-x --source-path="$HAM_HOME" --source-path="$HAM_HOME/bin")

    if [[ "$LINT_DIFF" == "yes" ]]; then
      # Note: This generates a patch of suggested fixes that could be applied
      # with 'git apply'. Its not perfect though so we dont use it in 'fix'
      # mode atm.
      SHELLCHECK_PARAMS=(--format=diff "${SHELLCHECK_PARAMS[@]}")
    elif [[ "$LINT_VERBOSE" != "yes" ]]; then
      SHELLCHECK_PARAMS=(--format=gcc "${SHELLCHECK_PARAMS[@]}")
    fi

    (
      set -x
      "$HAM_SHELL_LINTER_DIR/shellcheck" "${SHELLCHECK_PARAMS[@]}" "${FILES[@]}"
    )
  fi
}

function ni_lint() {
  local DIR
  DIR=$(pwd)

  local FILES=()
  if [ -z "$1" ]; then
    while IFS='' read -r line; do
      FILES+=("$line")
    done < <(find . -name "*.ni" -o -name "*.nip" -o -name "*.niw")
  else
    FILES=("$@")
  fi

  local INCLUDE_PARAMS=()
  for dir in "${NI_INCLUDE_DIRS[@]}"; do
    if [ -d "$dir" ]; then
      INCLUDE_PARAMS+=(-I"$dir")
    fi
  done

  for f in "${FILES[@]}"; do
    ni-lint "${INCLUDE_PARAMS[@]}" "$f"
  done
}

function ham_flymake_lint() {
  if [[ "$NO_LINT" == "yes" ]]; then
    return 0
  fi

  local FILEPATH
  FILEPATH=$(abspath "$1")

  # ham-flymake FLYMAKE=1 CHK_SOURCES=_Test_TiledWidget_aflymake.cpp FLYMAKE_BASEDIR=./ check-syntax
  local DIR
  DIR=$(dirname "$FILEPATH")
  local FILENAME
  FILENAME=$(basename "$FILEPATH")

  local BUILD_HAM_PATH
  BUILD_HAM_PATH=$(ham-find-file-up _build.ham "$DIR")
  if [ -z "$BUILD_HAM_PATH" ]; then
    log_error "Can't find _build.ham for '$FILENAME' in '$DIR'."
    return 1
  fi
  log_info "BUILD_HAM_PATH: '$BUILD_HAM_PATH'"

  local BUILD_HAM_DIR
  BUILD_HAM_DIR=$(dirname "$BUILD_HAM_PATH")
  log_info "BUILD_HAM_DIR: $BUILD_HAM_DIR"

  local REL_FILEPATH
  REL_FILEPATH=$(path_fwdslash "$(coreutils realpath --relative-to="$BUILD_HAM_DIR" "$FILEPATH")")
  log_info "REL_FILEPATH: $REL_FILEPATH"

  # We can't just rename the file because Sublime/VScode won't be able to map
  # the file to the original one. We could add a #line directive in the
  # renamed file but it's not worth it as their's probably edge cases where
  # it won't work and be a pain to track down.
  #
  # FLYMAKE_FILENAME=_${FILENAME%.*}_aflymake.${FILENAME##*.}
  (
    set -ex
    cd "$DIR"
    ham-flymake FLYMAKE=1 CHK_SOURCES="$REL_FILEPATH" FLYMAKE_BASEDIR="$BUILD_HAM_DIR" check-syntax
  )
}

function cpp_format_dir() {
  local DIR
  DIR=$1
  shift
  log_info "cpp_format_dir: '$DIR'"
  (
    set -e
    # Note we exclude extensions that are reserved for code
    # generation: .cxx, .hxx, .idl.inl
    find "$DIR" \
      \( -name '*.c' \
      -o -name '*.cc' \
      -o -name '*.cpp' \
      -o -name '*.m' \
      -o -name '*.mm' \
      -o -name '*.h' \
      -o -name '*.hh' \
      -o -name '*.hpp' \
      -o -name '*.inl' \
      ! -iname '*.cxx' \
      ! -iname '*.hxx' \
      ! -iname '*.idl.inl' \
      \) -print0 |
      xargs -t -0 -n 8 -P "${HAM_NUM_JOBS:-8}" run-for-xargs ham-format-cpp "$@"
  ) || return 1
}

function cpp_lint() {
  if [ "$LINT_FORMAT" == "yes" ] || [ "$LINT_FIX" == "yes" ]; then
    PARAMS=(format)
  else
    PARAMS=(dryrun)
  fi

  if [ -z "$1" ]; then
    DIR=$(pwd)
    if [ "$LINT_FORMAT" == "yes" ]; then
      cpp_format_dir "$DIR" "${PARAMS[@]}"
    fi
  elif [ -d "$1" ]; then
    DIR="$1"
    if [ "$LINT_FORMAT" == "yes" ]; then
      cpp_format_dir "$DIR" "${PARAMS[@]}"
    fi
  else
    if [ "$LINT_FORMAT" == "yes" ]; then
      (
        set -x
        ham-format-cpp "${PARAMS[@]}" "$1"
      )
    fi
    if [[ "$1" == *.c* ]]; then
      ham_flymake_lint "$1"
    else
      log_warning "Cant flymake header '$1'."
    fi
  fi
}

function java_clang_format_dir() {
  DIR=$1
  shift
  log_info "java_clang_format_dir: '$DIR'"
  (
    set -e
    find "$DIR" \
      \( -name '*.java' \
      \) -print0 |
      xargs -t -0 -n 10 -P "${HAM_NUM_JOBS:-8}" run-for-xargs ham-clang-format-cpp "$@"
  ) || return 1
}

function java_lint() {
  if [ "$LINT_FORMAT" == "yes" ] || [ "$LINT_FIX" == "yes" ]; then
    PARAMS=(format)
  else
    PARAMS=(dryrun)
  fi

  if [ -z "$1" ]; then
    DIR=$(pwd)
    java_clang_format_dir "$DIR" "${PARAMS[@]}"
  else
    (
      set -x
      ham-format-cpp "${PARAMS[@]}" "$1"
    )
  fi
}

function js_lint() {
  toolset_import_once nodejs >/dev/null
  FIX_OPT=
  if [ "$LINT_FIX" == "yes" ]; then
    FIX_OPT=--fix
  fi

  ESLINT_TARGET="${1:-$LINT_JS_DIR}"               # Use $1 or $LINT_JS_DIR
  ESLINT_TARGET="${ESLINT_TARGET:-./resources/js}" # If not specified default to ./resources/js
  (
    set -x
    eslint $FIX_OPT --max-warnings=0 --ext .js --ext .jsx --ext .mjs --ext .cjs "$ESLINT_TARGET"
  )
}

function php_lint() {
  toolset_import_once php >/dev/null
  if [ "$LINT_FIX" == "yes" ]; then
    if [ -z "$1" ]; then
      (
        set -x
        ./tools/php-cs-fixer/vendor/bin/php-cs-fixer fix .
      )
    else
      (
        set -x
        ./tools/php-cs-fixer/vendor/bin/php-cs-fixer fix "$1"
      )
    fi
  fi
  if [ -z "$1" ]; then
    (
      set -x
      ./vendor/bin/phpstan analyse --memory-limit=8000M
    )
  else
    (
      set -x
      ./vendor/bin/phpstan analyse --memory-limit=8000M --error-format=raw "$1"
    )
  fi
}

function rust_lint() {
  toolset_import_once rust >/dev/null

  # Allow some lints
  local CLIPPYFLAGS
  CLIPPYFLAGS=()
  # CLIPPYFLAGS=(-A clippy::bool_comparison ${CLIPPYFLAGS[@]})

  if [ "$LINT_FIX" == "yes" ]; then
    # Cargo fix can't be run on a single file...
    (
      set -x
      cargo clippy --fix --allow-dirty --allow-staged -- "${CLIPPYFLAGS[@]}"
    )
  else
    # Cargo clippy can't be run on a single file...
    (
      set -x
      cargo clippy -- "${CLIPPYFLAGS[@]}"
    )
  fi

  # Format after
  if [ "$LINT_FORMAT" == "yes" ]; then
    if [ -z "$1" ]; then
      (
        set -x
        cargo fmt
      )
    else
      (
        set -x
        rustfmt "$1"
      )
    fi
  fi
}

function lint_file() {
  local CMD
  CMD="$1"
  case "$CMD" in
    *.cxx | *.hxx | *.idl.inl | *.idl.*)
      log_warning "cxx, hxx & idl.* extensionsa are reserved for generated code and shouldnt be linted for '$CMD'."
      ;;
    *.c | *.cc | *.cpp | *.h | *.hh | *.hpp | *.inl | *.mm | *.m)
      cpp_lint "$CMD"
      errcheck $? cpp_lint "Single cpp file lint failed for '$CMD'."
      ;;
    *.cni | *.cpp2)
      ham_flymake_lint "$CMD"
      errcheck $? cpp_lint "Single cpp2 file lint failed for '$CMD'."
      ;;
    *.java)
      java_lint "$CMD"
      errcheck $? cpp_lint "Single java file lint failed for '$CMD'."
      ;;
    *.php)
      php_lint "$CMD"
      errcheck $? php_lint "Single php file lint failed for '$CMD'."
      ;;
    *.rs)
      rust_lint "$CMD"
      errcheck $? rust_lint "Single rust file lint failed for '$CMD'."
      ;;
    *.js | *.jsx | *.mjs | *.cjs)
      js_lint "$CMD"
      errcheck $? js_lint "Single js file lint failed for '$CMD'."
      ;;
    *.ni | *.nip | *.niw)
      ni_lint "$CMD"
      errcheck $? ni_lint "Single ni file lint failed for '$CMD'."
      ;;
    *.sh | *."")
      sh_lint "$CMD"
      errcheck $? sh_lint "Single sh file lint failed for '$CMD'."
      ;;
    *.ham)
      build_ham_lint "$CMD"
      errcheck $? build_ham_lint "Single ham file lint failed for '$CMD'."
      ;;
    *)
      die "Unsupported file type for '$CMD'."
      ;;
  esac
}

function build_ham_lint() {
  hamx "$1" || return 1
}

function lint_dir() {
  LANG=$1
  DIRNAME=$2
  if [ ! -d "$DIRNAME" ]; then
    log_error "'$DIRNAME' is not a directory, after '${LANG}'."
    return 1
  fi
  shift
  log_info "${LANG}_lint all in '$DIRNAME'."
  (
    cd "$DIRNAME"
    "${LANG}_lint"
    errcheck $? "${LANG}_lint" "${LANG} files lint failed in '$DIRNAME'."
  )
}

function lint_dir_or_file() {
  LANG=$1
  DIR_OR_FILENAME=$2
  if [ -d "$DIR_OR_FILENAME" ]; then
    log_info "lint_dir_or_file: $LANG: formatting directory '$DIR_OR_FILENAME'."
    lint_dir "$LANG" "$DIR_OR_FILENAME"
  elif [ -e "$DIR_OR_FILENAME" ]; then
    "${LANG}_lint" "$DIR_OR_FILENAME"
  else
    log_warning "lint_dir_or_file: $LANG: '$DIR_OR_FILENAME' doesn't exist."
  fi
}

function all_lint() {
  if [ -z "$1" ]; then
    log_error "Nothing to lint specified in '$DIRNAME'."
    return 1
  fi

  while [ "$1" != "" ]; do
    CMD="$1"
    shift
    if [ -d "$CMD" ]; then
      log_error "Specified directory '$CMD' should be preceeded by the language to lint."
      return 1
    fi
    DIR=$1
    case "$CMD" in
      cpp)
        lint_dir_or_file cpp "$DIR"
        errcheck $? all_lint "lint_file cpp '$DIR'." || return 1
        shift # used DIR=$1, so shift
        ;;
      java)
        lint_dir_or_file java "$DIR"
        errcheck $? all_lint "lint_file java '$DIR'." || return 1
        shift # used DIR=$1, so shift
        ;;
      php)
        lint_dir_or_file php "$DIR"
        errcheck $? all_lint "lint_file php '$DIR'." || return 1
        shift # used DIR=$1, so shift
        ;;
      js)
        lint_dir_or_file js "$DIR"
        errcheck $? all_lint "lint_file js '$DIR'." || return 1
        shift # used DIR=$1, so shift
        ;;
      rust)
        lint_dir_or_file rust "$DIR"
        errcheck $? all_lint "lint_file rust '$DIR'." || return 1
        shift # used DIR=$1, so shift
        ;;
      ni)
        lint_dir_or_file ni "$DIR"
        errcheck $? all_lint "lint_file ni '$DIR'." || return 1
        shift # used DIR=$1, so shift
        ;;
      sh)
        lint_dir_or_file sh "$DIR"
        errcheck $? all_lint "lint_file sh '$DIR'." || return 1
        shift # used DIR=$1, so shift
        ;;
      *)
        lint_file "$CMD"
        errcheck $? all_lint "lint_file '$CMD' failed." || return 1
        ;;
    esac
  done
}

function ham_lint_fix_usage() {
  echo "usage:"
  echo "  ${SCRIPT_NAME} modes FILE"
  echo "    Check a single file."
  echo "  ${SCRIPT_NAME} modes language DIRECTORY"
  echo "    Check all the files of the specified language in the specified directory."
  echo ""
  echo "modes:"
  echo "  --lint          Lint the code."
  echo "  --fix           If possible fix the problems detected during linting."
  echo "  --format        If possible format the specified files."
  echo "  --check-format  Only check if the code would be formatted."
  echo "  --verbose       Use the most verbose output (meant for cli, not tools integration)."
  echo "  --diff          When available use a diff format for the fixes without applying any."
  echo ""
  echo "examples:"
  echo "  ${SCRIPT_NAME} sources/MyModule/MyThing.cpp"
  echo "  ${SCRIPT_NAME} cpp sources/MyModule/"
  exit 1
}

function ham_lint_fix_main() {
  export NO_LINT=yes
  if [[ "$1" != "-"* ]]; then
    log_error "No mode specified."
    ham_lint_fix_usage
  fi
  while [[ "$1" == "-"* ]]; do
    if [ "$1" == "--fix" ]; then
      shift
      export LINT_FIX=yes
    elif [ "$1" == "--format" ]; then
      shift
      export LINT_FORMAT=yes
    elif [ "$1" == "--check-format" ]; then
      shift
      export LINT_CHECK_FORMAT=yes
    elif [ "$1" == "--lint" ]; then
      shift
      export NO_LINT=
    elif [ "$1" == "--verbose" ]; then
      shift
      export LINT_VERBOSE=yes
    elif [ "$1" == "--diff" ]; then
      shift
      export LINT_DIFF=yes
    else
      log_error "Unknown option '$1'."
      ham_lint_fix_usage
    fi
  done

  if [ -z "$1" ]; then
    log_error "No path to lint specified."
    ham_lint_fix_usage
  else
    all_lint "$@"
  fi
}

function ham_lint_fix_format_sh_main() {
  if [[ ! -e "${HAM_LINT_ALL_FILENAME}" ]]; then
    log_error "'$(abspath ${HAM_LINT_ALL_FILENAME})' doesn't exist."
    exit 1
  fi

  local PARAMS=()
  if [[ "$1" == "fix" ]]; then
    PARAMS=(--fix --format)
  elif [[ "$1" == "format" ]]; then
    PARAMS=(--format)
  elif [[ "$1" == "lint" ]]; then
    PARAMS=(--lint --check-format)
  elif [[ "$1" == "fixlint" ]]; then
    PARAMS=(--fix --format --lint)
  else
    die ham_lint_fix_format_sh_main "No mode specified: lint|fix|format|fixlint"
  fi
  shift

  if [[ -n "$1" ]]; then
    ham_lint_fix_main "${PARAMS[@]}" "$@"
  else
    local IFS_OLD=$IFS
    IFS=$' \n'
    local ALL_FILES=()
    while read -r line; do
      for word in $line; do
        ALL_FILES+=("$word")
      done
    done <"${HAM_LINT_ALL_FILENAME}"
    IFS=$IFS_OLD
    # log_debug "ALL_FILES: ${ALL_FILES[@]}"

    ham_lint_fix_main "${PARAMS[@]}" "${ALL_FILES[@]}"
  fi
}

function ham_lint_sh() {
  ham_lint_fix_format_sh_main lint "$@"
}

function ham_fix_sh() {
  ham_lint_fix_format_sh_main fix "$@"
}

function ham_format_sh() {
  ham_lint_fix_format_sh_main format "$@"
}

function ham_fixlint_sh() {
  ham_lint_fix_format_sh_main fix "$@"
}
