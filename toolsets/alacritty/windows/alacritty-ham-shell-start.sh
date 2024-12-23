HAM_HOME=K:/Work/ham
export HAM_HOME
if [ -e "$HOME/_alacritty_last_cwd.txt" ]; then
    ALACRITTY_LAST_CWD=$(cat "$HOME/_alacritty_last_cwd.txt")
fi
if [ -n "$ALACRITTY_LAST_CWD" ]; then
    echo "I/Alacritty starting in last cwd '$ALACRITTY_LAST_CWD'."
    cd "$ALACRITTY_LAST_CWD"
else
    echo "I/Alacritty starting in inherited working dir '$(pwd)'."
fi
source "$HAM_HOME/bin/ham-bash-start.sh"
