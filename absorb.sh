#!/bin/sh
# Absorbs a dotfile into this repo.

set -eu

[ -e apply.sh ] || die not in repo

die() {
	echo $*
	exit 1
}

for f in "$@"; do
	[ -e $f ] || die no such file: $f
	if [ -h $f ]; then
		# echo $f: already a symlink
		continue
	fi
	f=$(realpath $f)
	short=${f#$HOME}
	short=${short#/} # idk
	short=${short#/}
	short=${short#/}
	if [ -e home/$short ]; then
		die $short: already exists in the repo, quitting early
	fi
	mv -vn $f home/$short
	echo installdot $short >> apply.sh
done
