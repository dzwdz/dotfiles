alias ga "git add"
alias gc "git commit"
alias gca "git commit -a"
alias gp "git push"
alias gd "git diff"
alias gds "git diff --staged"
alias gs "git status -s"
alias gl "git log --reverse \"--pretty=%x1b[30m%h%x1b[0m %s %x1b[1;30m%ar%x1b[0m\""
alias cr "cd (git rev-parse --show-toplevel)"

alias todo "rg -i -e todo -e fixme"
alias vi nvim
alias s ssh
alias nw "tmux new-window"
alias ns "tmux split-window -b"

alias goodnight "sudo poweroff"
alias log "cd ~syncthing/Sync/log/; vi (date -I)"
alias irc "track irc mosh pi tmux a"

fish_vi_key_bindings

# TODO move to .profile
set CDPATH . /code ~/Documents ~
set GEM_HOME (ruby -e 'puts Gem.user_dir')
set PATH $PATH $GEM_HOME/bin ~/.bin ~/.local/bin ~/.local/opt/cross/bin ~/.pkgs/usr/bin /sbin
export EDITOR nvim
export FZF_DEFAULT_COMMAND="rg --hidden --files -g '!.git/'"

export GIB_HOME="/hdd/gib/"

source ~/.privaterc 2>/dev/null

source /usr/share/fzf/key-bindings.fish 2>/dev/null
if functions -q fzf_key_bindings
	fzf_key_bindings
end

function fish_title
	true
end

function fish_greeting
end
