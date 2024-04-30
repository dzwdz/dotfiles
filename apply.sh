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

installdot .Xmodmap
installdot .Xresources
installdot .Xsession
for file in .bin/*; do
	installdot $file
done
installdot .config/fish/config.fish
installdot .config/fish/functions/fish_prompt.fish
installdot .config/ncmpcpp/config
installdot .config/nvim/init.vim
installdot .emacs.d/init.el
installdot .gitconfig
installdot .local/share/fonts/BmPlus_IBM_VGA_8x16.otb
installdot .tmux.conf
installdot .vim/ftdetect/html.vim
installdot .vim/pack/ages/start/gruvbox
installdot .vim/pack/ages/start/vim-fugitive
installdot .vimrc
installdot .xbindkeysrc
installdot .xinitrc
installdot .xsessionrc
installdot .bash_aliases
