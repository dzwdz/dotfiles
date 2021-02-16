inoremap jk <ESC>
let mapleader = "'"

syntax on
set number
set noswapfile
set hlsearch
set ignorecase
set incsearch
set title
set mouse=a
set showcmd
set laststatus=1

set tabstop=4
set shiftwidth=4
set updatetime=100

set autoindent

set foldmethod=indent
hi Folded ctermbg=NONE

hi GitGutterAdd		ctermbg=NONE ctermfg=GREEN
hi GitGutterChange	ctermbg=NONE ctermfg=YELLOW
hi GitGutterDelete	ctermbg=NONE ctermfg=RED
hi SignColumn		ctermbg=NONE
let g:gitgutter_sign_modified = '~'

inoremap {<CR> {<CR><Tab><End><CR><BS>}<Up><Right>


" switch to last buffer
nnoremap <Leader><Leader> :e#<CR>

nnoremap <Leader>f :F<CR>

set rtp+=/usr/bin/fzf
let g:fzf_layout = { 'window': 'enew' }

let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_winsize = 20

nnoremap <C-s> :w<CR>
nnoremap <tab> :tabnext<CR>
nnoremap <S-tab> :tabprevious<CR>
nnoremap <C-w> :q<CR>
nnoremap <C-t> :Te<CR>

hi TabLineFill cterm=bold ctermbg=None
hi TabLine ctermfg=Black ctermbg=None cterm=none
hi TabLineSel ctermfg=3

" if vim is started with no arguments, start netrw
" autocmd StdinReadPre * let s:std_in=1
" autocmd VimEnter * if argc() == 0 && !exists('s:std_in') | E | endif

autocmd FileType ruby setlocal shiftwidth=2 tabstop=2 expandtab

runtime ftplugin/man.vim
