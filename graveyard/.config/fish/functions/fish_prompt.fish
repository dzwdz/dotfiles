set __fish_git_prompt_show_informative_status

function fish_prompt
	echo -n (basename (pwd))" "

	printf "\033[20G"
 
	echo -n (date +%H:%M)" @"(hostname)
	fish_git_prompt

	if jobs -q
		printf '\n  * '
	else
		printf '\n  : '
	end
end
