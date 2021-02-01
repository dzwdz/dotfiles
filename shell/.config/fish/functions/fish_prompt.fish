function fish_prompt
    set_color $fish_color_cwd
    echo -n (basename $PWD)
    set_color normal
	if jobs -q
		echo -n '* '
	else
	    echo -n ': '
	end
end

function fish_right_prompt
    set_color red
    echo -n "$PWD "
    set_color blue
    date +%H:%M
end
