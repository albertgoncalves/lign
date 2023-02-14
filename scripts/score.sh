#!/usr/bin/env bash

set -eu

"$WD/bin/main" "$WD/out/melody.ly"

lilypond -o "$WD/out" -dbackend=eps -ddelete-intermediate-files \
    -dinclude-eps-fonts -dno-gs-load-fonts -dpixmap-format=pngalpha --png \
    "$WD/out/melody.ly"
feh -B gray "$WD/out/melody.png" &
