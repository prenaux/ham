#!/bin/bash
export HAM_NO_VER_CHECK=1
if [[ -z "$HAM_HOME" ]]; then echo "E/HAM_HOME not set !"; exit 1; fi
. "$HAM_HOME/bin/ham-bash-setenv.sh"
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"
. hat ./_ham_project > /dev/null
set -e

# Source directory
DIR="src"

if [ -z "$1" ]; then
    # Use grep to find python files containing a main function
    # Use awk to print only the file names
    # Use fzf to let the user select a file
    FILE=$(grep -rl 'if __name__ == "__main__":' $DIR | awk -F/ '{print $NF}' | fzf)

    # Check if a file was selected
    if [ -z "$FILE" ]
    then
        echo "I/No script selected."
    else
        echo "I/Running $FILE"
        python3 "$DIR/$FILE"
    fi
else
    FILE="$1"
    if [ ! -e "$DIR/$FILE" ]; then
        FILE="$1.py"
    fi
    if [ ! -e "$DIR/$FILE" ]; then
        log_error
        exit 1
    fi
    shift
    python3 "$DIR/$FILE" "$@"
fi
