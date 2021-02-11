alias py python

alias gs "git status -s"
alias gc "git commit -m"
alias gca "git commit -am"
alias gp "git push"
alias gl "git log --pretty=oneline --abbrev-commit --reverse"

alias nw "urxvt &; disown"
alias :q exit
alias sf "seinfeld mark"
alias idea "dm idea.sh ."

set fish_color_command red
set fish_color_param brblack
set fish_color_operator green
set fish_color_autosuggestion cyan
set fish_color_error white
set fish_color_search_match --background='000'

set CDPATH . /hdd/repo/current ~/Documents ~
set GEM_HOME (ruby -e 'puts Gem.user_dir')
set PATH $PATH $GEM_HOME/bin ~/opt/cross/bin/ /opt/idea-IC-202.7319.50/bin /hdd/repo/current/utils
set EDITOR vim
