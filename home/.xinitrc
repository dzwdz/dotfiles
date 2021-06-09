#!/bin/sh

export _JAVA_AWT_WM_NONREPARENTING=1
export LD_LIBRARY_PATH=.pkgs/usr/lib/

xrdb ~/.Xresources
xmodmap ~/.Xmodmap
setxkbmap -layout pl 
xbindkeys
(sleep 10; xmodmap ~/.Xmodmap; setxkbmap -layout pl) &

# eval $(/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh)
# export GNOME_KEYRING_CONTROL GNOME_KEYRING_PID GPG_AGENT_INFO SSH_AUTH_SOCK

# TODO broken
xss-lock --transfer-sleep-lock -- i3lock --nofork -e -c 000000 &

dex -ae dwm &
flameshot &
unclutter &
picom &
~/.fehbg

pulseaudio --exit-idle-time=-1 &
mpdscribble &
mpd &

tmux new-session -d -s main
tmux new-session -d -s irc mosh pi tmux a
tmux new-session -d -s music ncmpcpp

emacs &
st -c term9 tmux attach-session -t music &
st -c term9 tmux attach-session -t irc &

# ~/.localfixes
exec dbus-run-session /hdd/builds/dwm/dwm
