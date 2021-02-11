inoremap jk <ESC>
let mapleader = "'"

syntax on
set number
set noswapfile
set hlsearch
set ignorecase
set incsearch
set title

hi GitGutterAdd		ctermbg=NONE ctermfg=GREEN
hi GitGutterChange	ctermbg=NONE ctermfg=YELLOW
hi GitGutterDelete	ctermbg=NONE ctermfg=RED
hi SignColumn		ctermbg=NONE
let g:gitgutter_sign_modified = '~'

set tabstop=4
set shiftwidth=4
set updatetime=100

set autoindent

inoremap {<CR> {<CR><Tab><End><CR><BS>}<Up><Right>

set mouse=a

set foldmethod=indent
hi Folded ctermbg=NONE

" switch to last buffer
nnoremap <Leader><Leader> :e#<CR>

let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_winsize = 20

nnoremap <C-s> :w<CR>
nnoremap <C-i> :tabnext<CR>
nnoremap <C-S-i> :tabprevious<CR>
nnoremap <C-w> :q<CR>
nnoremap <C-t> :Te<CR>

hi TabLineFill cterm=bold ctermbg=None
hi TabLine ctermfg=White ctermbg=None

" if vim is started with no arguments, start netrw
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists('s:std_in') | E | endif

autocmd FileType ruby setlocal shiftwidth=2 tabstop=2 expandtab

runtime ftplugin/man.vim
