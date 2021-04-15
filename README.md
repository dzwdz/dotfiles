# dotfiles
files with dots. this repo was never meant to be public, yet here we are

## csgo
csgo configs, need to be manually symlinked to the cfg directory

autostart > `exec symlink_name/bindy.cfg`

## desktop
uses:
- i3-gaps, i3blocks, feh, picom, dex
- rofi
- flameshot
- the Jetbrains Mono font
- urxvt
- jq (for the display swapper)

the power rofi thingy requires a sudoers modification, to allow running grub-reboot without a password

## shell
uses:
- fish
- ripgrep (for `tds`)

## wacom
uses:
- xsetwacom
- xbindkeys

## vim
uses:
- nvim (although the config should be compatible with vim too)
- fzf, ripgrep
- xclip
- vim-gitgutter
