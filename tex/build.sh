#!/bin/bash

shopt -s nullglob

cd "$(dirname "$(readlink -f "${0}")")"

case "${1}" in
    "")
        latexmk -latexoption="-shell-escape" -halt-on-error -xelatex main.tex
        ;;
    clean)
        latexmk -C
        for f in *.bbl *.run.xml *.auxlock *-figure*.{pdf,log,dpth,md5}; do
            rm -rvf "${f}"
        done
        ;;
    watch)
        latexmk -halt-on-error -xelatex -pvc main.tex
        ;;
    *)
        echo "what?"
        ;;
esac
