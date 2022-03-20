#!/bin/sh

set -eux

BASE=/usr/busybox/bin
BB_PATH=$(which busybox)
mkdir -p $BASE

for util in $(busybox --list); do
	ln -s $BB_PATH $BASE/$util
done
