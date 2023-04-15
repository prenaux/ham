#!/bin/bash -e
export HAM_NO_VER_CHECK=1
if [[ -z "$HAM_HOME" ]]; then echo "E/HAM_HOME not set !"; exit 1; fi
. "$HAM_HOME/bin/ham-bash-setenv.sh"

function ni_lint() {
    DIR=`pwd`
    if [ -z "$1" ]; then
        FILES=$(find . -name "*.ni" -o -name "*.nip" -o -name "*.niw")
    else
        FILES="$@"
    fi

    for f in $FILES
    do
        ni -c "$DIR/$f"
    done
}

function ham_flymake_lint() {
    # ham-flymake FLYMAKE=1 CHK_SOURCES=_Test_TiledWidget_aflymake.cpp FLYMAKE_BASEDIR=./ check-syntax
    DIR=$(dirname $1)
    FILENAME=$(basename "$1")
    # We can't just rename the file because VScode won't be able to map the file
    # to the original one. We could add a #line directive in the renamed file
    # but it's not worth it as their's probably edge cases where it won't work
    # and be a pain to track down.
    # FLYMAKE_FILENAME=_${FILENAME%.*}_aflymake.${FILENAME##*.}
    (set -ex;
     cd "$DIR"
     # cp "$FILENAME" "$FLYMAKE_FILENAME"
     touch "$FILENAME"
     ham-flymake FLYMAKE=1 CHK_SOURCES="$FILENAME" FLYMAKE_BASEDIR="./" check-syntax
     # rm "$FLYMAKE_FILENAME"
     )
}

function cpp_lint_dir() {
    DIR=$1
    shift
    echo "I/cpp_lint_dir: '$DIR'"
    (set -e ;
     find "$DIR" \
          \( -name '*.c' \
          -o -name '*.cc' \
          -o -name '*.cpp' \
          -o -name '*.h' \
          -o -name '*.hh' \
          -o -name '*.hpp' \
          -o -name '*.inl' \
          ! -iname '*.idl.inl' \
          \) -print0 |\
         xargs -t -0 -n 10 -P ${HAM_NUM_JOBS:-8} run-for-xargs clang-format $@) || return 1
}

function cpp_lint() {
    if [ "$LINT_FORMAT" == "yes" ] || [ "$LINT_FIX" == "yes" ]; then
        PARAMS=(-i)
    else
        PARAMS=(--dry-run -Werror)
    fi

    if [ -z "$1" ]; then
        DIR=`pwd`
        cpp_lint_dir "$DIR" ${PARAMS[@]}
    else
        (set -x; clang-format ${PARAMS[@]} "$1")
        ham_flymake_lint "$1"
    fi
}

function js_lint() {
    toolset_import_once nodejs > /dev/null
    FIX_OPT=
    if [ "$LINT_FIX" == "yes" ]; then
        FIX_OPT=--fix
    fi

    ESLINT_TARGET="${1:-$LINT_JS_DIR}" # Use $1 or $LINT_JS_DIR
    ESLINT_TARGET="${ESLINT_TARGET:-./resources/js}" # If not specified default to ./resources/js
    (set -x; eslint $FIX_OPT --max-warnings=0 --ext .js --ext .jsx --ext .mjs --ext .cjs "$ESLINT_TARGET")
}

function php_lint() {
    toolset_import_once php > /dev/null
    if [ "$LINT_FIX" == "yes" ]; then
        if [ -z "$1" ]; then
            (set -x; ./tools/php-cs-fixer/vendor/bin/php-cs-fixer fix .)
        else
            (set -x; ./tools/php-cs-fixer/vendor/bin/php-cs-fixer fix "$1")
        fi
    fi
    if [ -z "$1" ]; then
        (set -x; ./vendor/bin/phpstan analyse --memory-limit=8000M)
    else
        (set -x; ./vendor/bin/phpstan analyse --memory-limit=8000M --error-format=raw "$1")
    fi
}

function rust_lint() {
    toolset_import_once rust > /dev/null

    # Allow some lints
    # CLIPPYFLAGS="-A clippy::bool_comparison"

    if [ "$LINT_FIX" == "yes" ]; then
        # Cargo fix can't be run on a single file...
        (set -x; cargo clippy --fix --allow-dirty --allow-staged -- $CLIPPYFLAGS)
    else
        # Cargo clippy can't be run on a single file...
        (set -x; cargo clippy -- $CLIPPYFLAGS)
    fi

    # Format after
    if [ "$LINT_FORMAT" == "yes" ]; then
        if [ -z "$1" ]; then
            (set -x; cargo fmt)
        else
            (set -x; rustfmt "$1")
        fi
    fi
}

function lint_file() {
    CMD="$1"
    EXT="${CMD##*.}"
    case "$EXT" in
        c|cc|cpp|cxx|h|hh|hpp|hxx|inl)
            cpp_lint "$CMD"
            errcheck $? cpp_lint "Single cpp file lint failed in '$DIRNAME'."
            ;;
        cni|cpp2)
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
        js|jsx|mjs|cjs)
            js_lint "$CMD"
            errcheck $? js_lint "Single js file lint failed in '$DIRNAME'."
            ;;
        ni|nip|niw)
            ni_lint "$CMD"
            errcheck $? ni_lint "Single ni file lint failed in '$DIRNAME'."
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
        echo "E/no directory '$DIRNAME' specified after '${LANG}'."
        return 1
    fi
    shift
    echo "I/${LANG}_lint all in '$DIRNAME'."
    pushd "$DIRNAME" >> /dev/null
    ${LANG}_lint
    errcheck $? ${LANG}_lint "${LANG} files lint failed in '$DIRNAME'."
    popd >> /dev/null
}

function all_lint() {
    if [ -z "$1" ]; then
        echo "E/Nothing to lint specified in '$DIRNAME'."
        return 1
    fi

    while [ "$1" != "" ]; do
        CMD="$1"
        shift
        case "$CMD" in
            cpp)
                (set -e ; lint_dir cpp $1)
                shift
                ;;
            php)
                (set -e ; lint_dir php $1)
                shift
                ;;
            js)
                (set -e ; lint_dir js $1)
                shift
                ;;
            rust)
                (set -e ; lint_dir rust $1)
                shift
                ;;
            ni)
                (set -e ; lint_dir ni $1)
                shift
                ;;
            *)
                lint_file $CMD
                ;;
        esac
    done
}

function lint_sh_usage() {
    echo "usage: ./_lint.sh (options) FILE_PATHS"
    echo ""
    echo "  --fix     If possible fix the problems detected during linting."
    echo "  --format  If possible format the specified files."
    echo ""
    echo "  A directory can be checked at once by specifying the language"
    echo "  follow by the directory path"
    echo ""
    echo "examples:"
    echo "  ./_lint.sh sources/MyModule/MyThing.cpp"
    echo "  ./_lint.sh cpp sources/MyModule/"
    exit 1
}

function lint_sh_main() {
    while [[ "$1" == "-"* ]]; do
        if [ "$1" == "--fix" ]; then
            shift
            export LINT_FIX=yes
        elif [ "$1" == "--format" ]; then
            shift
            export LINT_FORMAT=yes
        else
            die "Unknown lint option '$1'."
        fi
    done

    if [ -z "$1" ]; then
        lint_sh_usage
    else
        all_lint "$@"
    fi
}
