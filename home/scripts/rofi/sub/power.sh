#!/bin/sh

case $1 in
	"Shutdown")
		systemctl poweroff
		;;
	"Reboot to Windows")
		sudo grub-reboot 2
		systemctl reboot
		;;
	*)	
		echo Shutdown
		echo Reboot to Windows
		;;
esac
