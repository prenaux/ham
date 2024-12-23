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
    if [[ "$LINT_CHECK_FORMAT" == "yes" ]]; then
      SHFMT_PARAMS=(-d "${SHFMT_PARAMS[@]}")
    else
      SHFMT_PARAMS=(-w "${SHFMT_PARAMS[@]}")
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

  for f in "${FILES[@]}"; do
    ni -c "$DIR/$f"
  done
}

function ham_flymake_lint() {
  if [[ "$NO_LINT" == "yes" ]]; then
    return 0
  fi

  # ham-flymake FLYMAKE=1 CHK_SOURCES=_Test_TiledWidget_aflymake.cpp FLYMAKE_BASEDIR=./ check-syntax
  local DIR
  DIR=$(dirname "$1")
  local FILENAME
  FILENAME=$(basename "$1")
  # We can't just rename the file because VScode won't be able to map the file
  # to the original one. We could add a #line directive in the renamed file
  # but it's not worth it as their's probably edge cases where it won't work
  # and be a pain to track down.
  # FLYMAKE_FILENAME=_${FILENAME%.*}_aflymake.${FILENAME##*.}
  (
    set -ex
    cd "$DIR"
    # cp "$FILENAME" "$FLYMAKE_FILENAME"
    touch "$FILENAME"
    ham-flymake FLYMAKE=1 CHK_SOURCES="$FILENAME" FLYMAKE_BASEDIR="./" check-syntax
    # rm "$FLYMAKE_FILENAME"
  )
}

function cpp_clang_format_dir() {
  local DIR
  DIR=$1
  shift
  log_info "cpp_clang_format_dir: '$DIR'"
  (
    set -e
    find "$DIR" \
      \( -name '*.c' \
      -o -name '*.cc' \
      -o -name '*.cpp' \
      -o -name '*.cxx' \
      -o -name '*.h' \
      -o -name '*.hh' \
      -o -name '*.hpp' \
      -o -name '*.hxx' \
      -o -name '*.inl' \
      ! -iname '*.idl.inl' \
      \) -print0 |
      xargs -t -0 -n 10 -P "${HAM_NUM_JOBS:-8}" run-for-xargs ham-clang-format-cpp "$@"
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
    cpp_clang_format_dir "$DIR" "${PARAMS[@]}"
  else
    (
      set -x
      ham-clang-format-cpp "${PARAMS[@]}" "$1"
      ham_flymake_lint "$1"
    )
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
      ham-clang-format-cpp "${PARAMS[@]}" "$1"
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
  local EXT
  EXT=$(path_extension "${CMD}")
  case "$EXT" in
    c | cc | cpp | cxx | h | hh | hpp | hxx | inl)
      cpp_lint "$CMD"
      errcheck $? cpp_lint "Single cpp file lint failed in '$DIRNAME'."
      ;;
    java)
      java_lint "$CMD"
      errcheck $? cpp_lint "Single java file lint failed in '$DIRNAME'."
      ;;
    cni | cpp2)
      ham_flymake_lint "$CMD"
      errcheck $? cpp_lint "Single cpp2 file lint failed in '$DIRNAME'."
      ;;
    php)
      php_lint "$CMD"
      errcheck $? php_lint "Single php file lint failed in '$DIRNAME'."
      ;;
    rs)
      rust_lint "$CMD"
      errcheck $? rust_lint "Single rust file lint failed in '$DIRNAME'."
      ;;
    js | jsx | mjs | cjs)
      js_lint "$CMD"
      errcheck $? js_lint "Single js file lint failed in '$DIRNAME'."
      ;;
    ni | nip | niw)
      ni_lint "$CMD"
      errcheck $? ni_lint "Single ni file lint failed in '$DIRNAME'."
      ;;
    sh | "")
      sh_lint "$CMD"
      errcheck $? sh_lint "Single sh file lint failed in '$DIRNAME'."
      ;;
    *)
      die "Unsupported extension '$EXT' for '$CMD' in '$DIRNAME'."
      ;;
  esac
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
