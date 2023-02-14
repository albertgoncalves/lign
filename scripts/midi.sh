#!/usr/bin/env bash

set -eu

timidity "$WD/out/melody.midi" -Ow -o "$WD/out/melody.wav"
aplay "$WD/out/melody.wav" &
