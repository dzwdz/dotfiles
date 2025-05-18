#!/bin/sh
# The current iteration of this script is inspired by elly's dotfiles:
#   https://git.sr.ht/~elly/dotfiles/tree/master/item/install.sh

set -eu

cd home
PWD=$(pwd)

die() {
	echo $*
	exit 1
}

installdot() {
	[ $# -eq 1 ] || die installdot: wrong amt of arguments, got $#
	target=~/$1
	loc=$PWD/$1
	[ -e $loc ] || die no such file in repo: $1
	
	if [ -e $target ]; then
		if [ -h $target ]; then
			prev=$(readlink $target)
			if [ $prev = $loc ]; then
				# echo $1: already linked
				return
			else
				echo $1: link to $prev
			fi
		fi
		if [ -e $target.old ]; then
			die $1: manual intervention needed, $target.old already exists
		fi
		echo $1: making a copy and linking
		mv -n $target $target.old
	else
		dir=$(dirname $target)
		if [ -d $dir ]; then
			echo $1: didn\'t exist
		else
			echo $1: not even $dir exists
			mkdir -p $dir
		fi
	fi
	ln -s $loc $target
}

installdots() {
	for f in "$@"; do
		installdot $f
	done
}

# X11
installdots .Xmodmap .XCompose .xbindkeysrc # keyboard config
installdots .Xresources .config/awesome # theming
installdots .config/i3
installdots .Xsession .xsessionrc .xinitrc # misc
installdots .config/rofi

# regular cli stuff
installdots .local/bin/*
installdots .config/fish/config.fish
installdots .config/git/config
installdots .config/tmux/tmux.conf
installdots .bash_aliases

# editors
installdots .vim/pack/ages/start/gruvbox
installdots .vim/pack/ages/start/vim-fugitive
installdots .vimrc
installdots .emacs.d/init.el
