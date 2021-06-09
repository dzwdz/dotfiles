#!/bin/sh
if [ -e /tmp/compact_mode ]
then
	polybar-msg cmd show
#	i3-msg gaps inner all set 4
	rm /tmp/compact_mode
else
	polybar-msg cmd hide
	i3-msg gaps inner all set 0
	touch /tmp/compact_mode
fi
