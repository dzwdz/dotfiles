function fish_prompt
	if set -q SSH_CLIENT
		echo -n (hostname)\ 
	end

    set_color --bold red
    echo -n (basename $PWD)
	set_color normal
	if jobs -q
		echo -n '*'
	else
	    echo -n ':'
	end
end

function fish_right_prompt
    set_color grey
	fish_git_prompt
	echo -n ' '
    date +%H:%M
	set_color normal
end
