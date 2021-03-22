function fish_greeting
	if test -e /tmp/cast_mode
		echo "cast"
	else
		if type -q sein
			sein compact
		end
	end
end
