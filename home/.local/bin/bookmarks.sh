#!/bin/bash
# Skrypt do nadpisywania spisu treści w PDFie.
# Częściowo na bazie https://github.com/SiddharthPant/booky.
# bookmarks.sh input.pdf input.txt

# TODO:
# - usuwanie stron: x 10

set -euo pipefail

die() {
	echo $*
	exit 1
}

checkfile() {
	if [ -e "$1" ]; then
		rm -i $1
	fi
	if [ -e "$1" ]; then
		die "not overriding $1, quitting"
	fi
}

input="$1"
data="$1.data"
txt="$2"
output="${input%.pdf}"_new.pdf

checkfile "$output"
checkfile "$data"

pdftk "$input" dump_data_utf8 output "$data"
sed -i '/^Bookmark/d' "$data" # pozbywamy się poprzednich zakładek

awk '
/^%offset/ {
	off = $2;
}
/^=/ {
	print "BookmarkBegin";
	print "BookmarkLevel:", length($1);
	print "BookmarkPageNumber:", $2 + off;
	$1=$2="";
	sub(/^\s*/, "");
	print "BookmarkTitle:", $0;
}
' "$txt" >> "$data"

pdftk "$input" update_info_utf8 "$data" output "$output"

rm -v $data
