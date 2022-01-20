#!/bin/sh

export _JAVA_AWT_WM_NONREPARENTING=1
export LD_LIBRARY_PATH=.pkgs/usr/lib/

xrdb ~/.Xresources
setxkbmap -layout pl 
xmodmap ~/.Xmodmap
xbindkeys

# eval $(/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh)
# export GNOME_KEYRING_CONTROL GNOME_KEYRING_PID GPG_AGENT_INFO SSH_AUTH_SOCK

xss-lock -l slock &

dex -ae dwm &
dwmstatus &
~/.fehbg

pipewire &
pipewire-pulse &
pulseaudio --exit-idle-time=-1 &
mpdscribble &
mpd &

eval $(ssh-agent)

tmux new-session -d -s irc mosh pi tmux a
tmux new-session -d -s music ncmpcpp

emacs &
st -c term9 tmux attach-session -t music &
st -c term9 tmux attach-session -t irc &

# ~/.localfixes

type dbus-run-session && exec dbus-run-session dwm
type dbus-launch && exec dbus-launch dwm
exec dwm
