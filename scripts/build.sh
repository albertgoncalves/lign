#!/usr/bin/env bash

set -eu

for x in bin build out; do
    if [ ! -d "$WD/$x" ]; then
        mkdir "$WD/$x"
    fi
done

flags=(
    -ccopt "-static"
    -nolabels
    -strict-formats
    -strict-sequence
    -unboxed-types
    -w "+1..67"
)

ocp-indent -i "$WD/src/"*.ml
cp "$WD/src/"*.ml "$WD/build/"

(
    cd "$WD/build/"
    ocamlc "${flags[@]}" "main.ml" -o "$WD/bin/main"
)
