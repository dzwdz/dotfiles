set prompt_cnt 0
set prompt_colors red f80 yellow green blue magenta

function fish_prompt
	set prompt_cnt (math $prompt_cnt % 6 + 1)
    set_color --bold $prompt_colors[$prompt_cnt]
	if set -q SSH_CLIENT
		echo -n (hostname)
		set_color -r brblue
	end
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
