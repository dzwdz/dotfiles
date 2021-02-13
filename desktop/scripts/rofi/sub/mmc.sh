#!/bin/sh

if [[ $1 ]]; then
	multimc -l "$1" </dev/null >/dev/null 2>/dev/null &
else
	ls ~/.local/share/multimc/instances | sed '/_MMC_TEMP/d;/instgroups.json/d'
fi
