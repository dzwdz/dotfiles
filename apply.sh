#!/usr/bin/env sh

# TODO ensure that this is run in the right dir
PREFIX=~

mkdir -p $PREFIX
ln -sr home/.bin $PREFIX/.bin
ln -sr home/.vim $PREFIX/.vim

for orig in $(git ls-files | grep home/)
do
	target=$PREFIX$(echo $orig | sed s/home//)
	mkdir -p $(dirname $target)
	ln -sr $orig $target
done
