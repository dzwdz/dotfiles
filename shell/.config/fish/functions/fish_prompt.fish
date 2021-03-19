function fish_prompt
    set_color -r --bold $fish_color_cwd
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
    set_color brmagenta
    echo $PWD
    set_color -r brblue
    date +%H:%M
	set_color normal
end

function fish_mode_prompt
  switch $fish_bind_mode
    case default
      set_color -r brgreen
      echo 'n'
    case insert
      set_color -r brblue
      echo 'i'
    case replace_one
      set_color -r green
      echo 'r'
    case visual
      set_color -r yellow
      echo 'v'
    case '*'
      set_color -r red
      echo '?'
  end
  set_color normal
end
