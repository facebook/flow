if [[ -n ${ZSH_VERSION-} ]]; then
  autoload -U +X bashcompinit && bashcompinit
fi

_flow ()
{
  CUR="${COMP_WORDS[COMP_CWORD]}"
  COMPREPLY=()
  OPTS=$(echo | flow shell-complete --current ${COMP_CWORD} -- ${COMP_WORDS[@]})

  if [ $? -ne 0 ]; then
    return $?
  fi

  if [ "$OPTS" = "FILE" ]; then
    COMPREPLY=( $(compgen -f -- ${CUR}) )
    return 0
  fi

  if [ "$OPTS" = "ARGUMENT" ]; then
    return 0
  fi

  COMPREPLY=( $(compgen -f -W "${OPTS}" -- ${CUR}) )
}
complete -F _flow -o filenames flow
