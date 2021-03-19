function fish_greeting
	if test -e /tmp/cast_mode
		echo "cast"
	else
		if type -q seinfeld
			seinfeld compact
		end
	end
end
