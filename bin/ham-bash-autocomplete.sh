#!/bin/bash

#
# Add this to your .bashrc:
#   if [[ -e "$HAM_HOME/bin/ham-bash-autocomplete.sh" ]]; then
#     . "$HAM_HOME/bin/ham-bash-autocomplete.sh"
#   fi
#
_ham_complete_ls_targets() {
  DIR=${1:-.}
  CUR_WORD="$2"
  (
    set -e
    cd "$DIR"
    find . -maxdepth 1 -type f -name "*.sh" -perm +111 -exec basename {} \;
    if [[ "$CUR_WORD" != _* ]] && [ -f "$DIR/_ham_project" ]; then
      ham-ls-targets
    fi
    echo "-T"
    echo "-D"
    echo "-X"
  )
}

_ham_autocomplete() {
  local cur_word prev_word completions

  # Get the current and previous words in the command line
  cur_word="${COMP_WORDS[COMP_CWORD]}"
  prev_word="${COMP_WORDS[COMP_CWORD - 1]}"

  # Loop through all words to find -D or -X arguments and store their folders
  local flag=false
  LAST_FOLDER=""
  for word in "${COMP_WORDS[@]}"; do
    if [[ "$word" == "-D" || "$word" == "-X" ]]; then
      flag=true
    elif $flag; then
      LAST_FOLDER=$word
      flag=false
    fi
  done

  # If -T get the list of toolsets. We don't autocomplete folder names to
  # discourage the usage of -T toolset as its not great, -X is more robust.
  if [[ "$prev_word" == "-T" ]]; then
    completions=$(cd "$HAM_HOME/specs/toolsets" && compgen -d -- "$cur_word")
  # If -D or -X was the last argument, get a list of subdirectories in the $WORK folder
  elif [[ "$prev_word" == "-D" || "$prev_word" == "-X" ]]; then
    completions=$(cd "$WORK" && compgen -d -- "$cur_word")
  elif [ -n "$LAST_FOLDER" ]; then
    # For other cases, use the output of _ham_complete_ls_targets as completions
    completions=$(_ham_complete_ls_targets "${WORK}/${LAST_FOLDER}" "$cur_word")
  else
    # For other cases, use the output of _ham_complete_ls_targets as completions
    completions=$(_ham_complete_ls_targets "." "$cur_word")
  fi

  # shellcheck disable=SC2207
  COMPREPLY=($(compgen -W "$completions" -- "$cur_word"))
}

complete -F _ham_autocomplete ham
