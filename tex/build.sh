#!/bin/bash

shopt -s nullglob

cd "$(dirname "$(readlink -f "${0}")")"

OPTS=(-latexoption="-shell-escape" -halt-on-error -xelatex)

case "${1}" in
    "")
        latexmk ${OPTS[@]} main.tex
        ;;
    clean)
        latexmk -C
        for f in *.bbl *.run.xml *.aux{,lock} *.log *.fls *-figure*.{pdf,log,dpth,md5}; do
            rm -rvf "${f}"
        done
        ;;
    watch)
        latexmk ${OPTS[@]} -pvc main.tex
        ;;
    *)
        echo "say whaaa?"
        ;;
esac
