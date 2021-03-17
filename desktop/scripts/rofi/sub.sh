#!/bin/sh

path=~/scripts/rofi/sub

stuff() {
    # wait for a lock on the rofi pidfile
    flock $XDG_RUNTIME_DIR/rofi.pid true
    rofi -show "sh:$path/$1"
}

if [[ $1 ]]; then
	stuff $1 </dev/null >/dev/null 2>/dev/null &
else
	ls $path
fi
