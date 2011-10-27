
_tog_complete () {
    local cur="${COMP_WORDS[COMP_CWORD]}"
    local commands="start stop status log report edit"
    local projects=( $( for i in $(ls ~/.tog) ; do echo $i ; done | grep ^[a-z] ) )

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

    # Find the number of non-"--" commands
    local num=0
    for word in ${COMP_WORDS[@]}
    do
        if [[ $word != -* ]]; then
            num=$((++num))
        fi
    done


    case "$prev" in
        start|log|edit)
            COMPREPLY=( $(compgen -W  "`echo ${projects[@]}`" -- ${cur}) )
            return
            ;;
    esac
}

complete -F _tog_complete tog
