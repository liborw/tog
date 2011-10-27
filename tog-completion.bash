
_tog_complete () {
    local cur="${COMP_WORDS[COMP_CWORD]}"
    local commands="start stop status log report edit"
    local projects=$(\ls ~/.tog/[a-z]*)

    # Subcommand list
    [[ ${COMP_CWORD} -eq 1 ]] && {
        COMPREPLY=( $(compgen -W  "${commands}" -- ${cur}) )
        return
    }

    # Find the first non-switch word
    local prev_index=1
    local prev="${COMP_WORDS[prev_index]}"
    while [[ $prev == -* ]]; do
        prev_index=$((++prev_index))
        prev="${COMP_WORDS[prev_index]}"
    done
}

complete -o bashdefault -o default -F _tog_complete tog
