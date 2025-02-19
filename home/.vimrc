" vim: foldmethod=marker : foldlevel=0

" Miscellaneous {{{
set encoding=utf8

set hlsearch incsearch ignorecase
command Nhide tabdo windo set nonumber 
command Nshow tabdo windo set number 
Nshow

set hidden
set noswapfile
set notitle
set mouse=a
set laststatus=1 showcmd

set path+=**

let g:neoterm_autoscroll = 1

if has('nvim')
	autocmd TermOpen * setlocal nonumber
endif
" }}}
" Theme {{{
syntax on
set background=dark

set colorcolumn=80
hi ColorColumn ctermbg=black

hi TabLineFill cterm=bold ctermbg=None
hi TabLine ctermfg=7 ctermbg=None cterm=none
hi TabLineSel ctermfg=3
hi LineNr ctermfg=7

hi VertSplit cterm=none
hi StatusLine cterm=bold
hi StatusLineNC cterm=bold

hi DiffAdd ctermbg=None
hi DiffDelete ctermbg=None

hi PMenu ctermbg=NONE ctermfg=none cterm=reverse

set list
set listchars=tab:·\ 
hi Whitespace ctermfg=DarkGray

set fillchars=fold:\ 

if has("gui_running")
	set go-=L go-=R go-=l go-=r go-=m go-=T go-=b go-=e
	set guicursor=a:blinkoff0
	let g:gruvbox_contrast_dark='hard'
	let g:fzf_colors = {}
	colorscheme gruvbox
	let g:fzf_colors.bg = ['fg', 'GruvboxBg0']
	set guifont=Iosevka\ Fixed\ 14

	let g:terminal_ansi_colors = [
		\'#282828', '#CC241D', '#98971A', '#D79921',
		\'#458588', '#B16286', '#689D6A', '#D65D0E',
		\'#fb4934', '#b8bb26', '#fabd2f', '#83a598',
		\'#d3869b', '#8ec07c', '#fe8019', '#FBF1C7',
	\]
	hi Terminal guibg='#1d2021' guifg='#ebdbb2'
	set title
endif
" }}}
" Indentation / Folding {{{
set ts=4 sw=4
set updatetime=100
set autoindent cindent

autocmd FileType ruby setlocal shiftwidth=2 tabstop=2 expandtab
autocmd FileType lisp setlocal shiftwidth=2 tabstop=2 expandtab
autocmd FileType scheme setlocal shiftwidth=2 tabstop=2 expandtab
autocmd FileType haskell setlocal shiftwidth=2 tabstop=2 expandtab
autocmd FileType txt setlocal foldmethod=indent
autocmd FileType markdown setlocal foldmethod=indent

set foldmethod=syntax
set foldlevel=99
hi Folded ctermbg=NONE
" }}}
" Mappings and custom commands {{{
let mapleader = " "

inoremap jk <ESC>
tnoremap jk <C-\><C-N> 

"inoremap {<CR> {<CR><Tab><End><CR><BS>}<Up><Right>
nnoremap <Leader><Leader> :e#<CR>
nnoremap <Leader>f :FZF<CR>
nnoremap <Leader>/ :let @/ = ""<CR>
nnoremap <Leader>r :w<CR>:!"%:p"<CR>

nnoremap <C-s> :w<CR>
nnoremap <tab> :tabnext<CR>
nnoremap <S-tab> :tabprevious<CR>
nnoremap <C-q> :bd<CR>
nnoremap !<C-q> :bd!<CR>
nnoremap gb :ls<CR>:b<space>

" launch manpages in native plugin
nmap K <Leader>K

nmap j gj
nmap k gk

nmap <silent> <c-k> :wincmd k<CR>
nmap <silent> <c-j> :wincmd j<CR>
nmap <silent> <c-h> :wincmd h<CR>
nmap <silent> <c-l> :wincmd l<CR>

nnoremap 0 ^

command! -nargs=1 -complete=help H help <args> | silent only

" make the current file executable
command! MakeX execute "!chmod +x \"%:p\""

iab rubysb #!/usr/bin/env ruby
iab vifold vim: foldmethod=marker : foldlevel=0
iab <expr> isonow strftime("%FT%T%z")
""" }}}
" Plugin specific {{{
"set rtp+=/usr/bin/fzf
"let g:fzf_layout = { 'window': 'enew' }
source /usr/share/doc/fzf/examples/fzf.vim

runtime ftplugin/man.vim

let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_winsize = 20
" }}}

augroup filetypedetect
  au! BufRead,BufNewFile *.sage,*.spyx,*.pyx setfiletype python
augroup END
