function fish_prompt
    set_color $fish_color_cwd
    echo -n (basename $PWD)
    set_color normal
    echo -n ': '
end

function fish_right_prompt
    set_color red
    date +%H:%M
end
