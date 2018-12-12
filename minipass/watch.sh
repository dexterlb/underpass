#!/bin/bash

rc=$(mktemp)
data=$(mktemp)
dummy=$(mktemp)

function writer {
    at_start=yes

    while read -d $'\n' line; do
        if [[ "${at_start}" == yes ]]; then
            echo -n '' > "${data}"
        fi

        echo "${line}" >> "${data}"

        if [[ "${line}" =~ "Press enter to force a rebuild" ]]; then
            at_start=yes
        else
            at_start=no
        fi
    done
}

function watcher {
    rm -rf "${dummy}"
    mkfifo "${dummy}"
    cat "${dummy}" | stack build --file-watch 2>&1 | writer
}

watcher &
watcher_pid="${!}"

function cleanup {
    rm -rvf "${rc}" "${data}" "${dummy}"
    kill "${watcher_pid}"
}

trap 'cleanup' INT TERM EXIT

cat << EOF > "${rc}"
set autoread

function! s:reload(...) abort
    bufdo e
    call Go()
endfunction

function! Go()
    call timer_start(200, function('s:reload'))
endfunction

call Go()
EOF

if which nvim &> /dev/null; then
    VIM=nvim
else
    VIM=vim
fi

"${VIM}" -u "${rc}" "${data}"