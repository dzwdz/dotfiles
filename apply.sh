#!/usr/bin/env sh
# TODO ensure that this is run in the right dir

PREFIX=~/
mkdir -p $PREFIX

# symlink files
for orig in $(git ls-files | grep home/)
do
	target=$PREFIX$(echo $orig | sed s/home//)
	mkdir -p $(dirname $target)
	rm -fv "$target"
	ln -sr $orig $target
done

# fix permissions on .ssh
chmod 700 $PREFIX/.ssh
chmod 644 $PREFIX/.ssh/config

# create some standard dirs
mkdir $PREFIX/art
mkdir $PREFIX/code
