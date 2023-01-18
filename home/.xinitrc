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

redshift -l manual:lat=50.15:lon=19.01 &

eval $(ssh-agent)

tmux new-session -d -s music ncmpcpp

emacs -T "Org Roam" ~/Notes/index.org &
st -c term9 tmux attach-session -t music &

# ~/.localfixes

type dbus-run-session && exec dbus-run-session dwm
type dbus-launch && exec dbus-launch dwm
exec dwm
