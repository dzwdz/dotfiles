inoremap jk <ESC>
let mapleader = "'"

" some general settings idk
syntax on
set background=dark
set encoding=utf8
set number
set noswapfile
set hlsearch
set ignorecase
set incsearch
set title
set mouse=a
set showcmd
set laststatus=1

set foldmethod=indent
set foldlevel=99
hi Folded ctermbg=NONE

set list
set listchars=tab:·\ 

hi TabLineFill cterm=bold ctermbg=None
hi TabLine ctermfg=Black ctermbg=None cterm=none
hi TabLineSel ctermfg=3


" indentation
set tabstop=4
set shiftwidth=4
set updatetime=100
set autoindent

autocmd FileType ruby setlocal shiftwidth=2 tabstop=2 expandtab


" useful bindings
inoremap {<CR> {<CR><Tab><End><CR><BS>}<Up><Right>
nnoremap <Leader><Leader> :e#<CR>
nnoremap <Leader>f :F<CR>
nnoremap <Leader>/ :let @/ = ""<CR>

nnoremap <C-s> :w<CR>
nnoremap <tab> :tabnext<CR>
nnoremap <S-tab> :tabprevious<CR>
nnoremap <C-w> :q<CR>
nnoremap <C-t> :Te<CR>

nnoremap 0 ^

" plugins
hi GitGutterAdd		ctermbg=NONE ctermfg=GREEN
hi GitGutterChange	ctermbg=NONE ctermfg=YELLOW
hi GitGutterDelete	ctermbg=NONE ctermfg=RED
hi SignColumn		ctermbg=NONE
let g:gitgutter_sign_modified = '~'

set rtp+=/usr/bin/fzf
let g:fzf_layout = { 'window': 'enew' }

runtime ftplugin/man.vim

let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_winsize = 20

