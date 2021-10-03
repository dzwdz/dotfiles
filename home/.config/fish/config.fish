alias ga "git add"
alias gc "git commit"
alias gca "git commit -a"
alias gs "git status -s"
alias gd "git diff"
alias gds "git diff --staged"
alias gp "git push"
alias gl "git log --pretty=oneline --abbrev-commit --shortstat --reverse | sed -E \"s/^[^ ]+/\\\\x1b[30m&\\\\x1b[m/\" | sed -zE \"s/\n [0-9]+ files? changed(, ([0-9]+) insertions?\(\+\))?(, ([0-9]+) deletions?\(\-\))?/ \\\\x1b[1;30m\+\2, -\4\\\\x1b[m/g\""
alias cr "cd (git rev-parse --show-toplevel)"

alias :q exit
alias sf "sein mark"
alias sc "sein compact"
alias todo "rg -i -e todo -e fixme"
alias vi nvim
alias s ssh

set __fish_git_prompt_show_informative_status

# TODO move to .profile
set CDPATH . /code ~/Documents ~
set GEM_HOME (ruby -e 'puts Gem.user_dir')
set PATH $PATH $GEM_HOME/bin ~/.bin ~/.local/bin ~/.local/opt/cross/bin ~/.pkgs/usr/bin /sbin
set EDITOR nvim
export FZF_DEFAULT_COMMAND="rg --hidden --files -g '!.git/'"



source /usr/share/doc/fzf/key-bindings.fish
if functions -q fzf_key_bindings
	fzf_key_bindings
end

function fish_title
	true
end

function fish_greeting
	tput cup 255 # put prompt at the bottom
end

function fish_prompt
	if set -q SSH_CLIENT
		echo -n (hostname)\ 
	end
    echo -n (basename $PWD)
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
