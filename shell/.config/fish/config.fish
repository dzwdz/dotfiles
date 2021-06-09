alias py python

alias gs "git status -s"
alias gc "git commit"
alias gca "git commit -a"
alias gp "git push"
alias ga "git add"
alias gl "git log --pretty=oneline --abbrev-commit --reverse"
alias cr "cd (git rev-parse --show-toplevel)"
alias tds "rg -i -e todo -e fixme"

alias nw "urxvt &; disown"
alias :q exit
alias sf "sein mark"
alias idea "dm idea.sh ."
alias nv nvim
alias s ssh

set fish_color_cwd brmagenta
set fish_color_command red
set fish_color_param foreground
set fish_color_operator green
set fish_color_autosuggestion cyan
set fish_color_error white
set fish_color_search_match --background='000'
set -g fish_term24bit 1
set __fish_git_prompt_show_informative_status

set CDPATH . /code ~/Documents ~
set GEM_HOME (ruby -e 'puts Gem.user_dir')
set PATH $PATH ~/go/bin $GEM_HOME/bin ~/opt/cross/bin/ /opt/idea-IC-202.7319.50/bin ~/.local/bin ~/.pkgs/usr/bin
set EDITOR nvim

export FZF_DEFAULT_COMMAND="rg --hidden --files --no-ignore-vcs -g '!.git/'"

source /usr/share/doc/fzf/key-bindings.fish
if functions -q fzf_key_bindings
	fzf_key_bindings
end
