# dotfiles
highly specific to my config n shit, has a bunch of hardcoded paths.
probably won't ever be public

## csgo
csgo configs, need to be manually symlinked to the cfg directory

autostart > `exec symlink_name/bindy.cfg`

## desktop
uses:
- i3-gaps, feh, , dex
- rofi
- polybar
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
