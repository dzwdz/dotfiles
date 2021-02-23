function fish_greeting
	if test -e /tmp/cast_mode
		echo "cast"
	else
		seinfeld compact
	end
end
