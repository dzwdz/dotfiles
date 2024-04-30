alias ga='git add'
alias gc='git commit'
alias gca='git commit -a'
alias gp='git push'
alias gd='git diff'
alias gds='git diff --staged'
alias gs='git status'
alias gss='git status -s'

alias s=ssh
alias g='grep -rIn'

ocr() {
	tesseract -l pol "$1" -
}

pdfcat() {
	pdftk "$@" cat output /dev/stdout
}
