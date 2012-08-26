#!bash
echo "Opening $1 at '$2'"
export RUN_DEBUGGER="${PROGRAMFILES}/Microsoft Visual Studio 9.0/Common7/IDE/devenv.exe"
if [ ! -f "$RUN_DEBUGGER" ]; then
    export RUN_DEBUGGER="${PROGRAMFILES}/Microsoft Visual Studio 10.0/Common7/IDE/devenv.exe"
    if [ ! -f "$RUN_DEBUGGER" ]; then
        export RUN_DEBUGGER=
    fi
fi
"${RUN_DEBUGGER}" -edit "$1" -command "Edit.GoTo $2" &
