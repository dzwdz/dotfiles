#!/bin/sh

if [ -z "$1"]; then
	ls /hdd/repo/current
else
	urxvt -cd "/hdd/repo/current/$1" &
fi
