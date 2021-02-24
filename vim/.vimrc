let mapleader = "'"

" some general settings idk
syntax on
set background=dark
set encoding=utf8

set hlsearch incsearch ignorecase
set number relativenumber

set hidden
set noswapfile
set title
set mouse=a
set laststatus=1 showcmd

set foldmethod=syntax
set foldlevel=99
hi Folded ctermbg=NONE

set list
set listchars=tab:·\ 

hi TabLineFill cterm=bold ctermbg=None
hi TabLine ctermfg=7 ctermbg=None cterm=none
hi TabLineSel ctermfg=3
hi LineNr ctermfg=7

autocmd FileType ruby setlocal shiftwidth=2 tabstop=2 expandtab
autocmd FileType txt setlocal foldmethod=indent
autocmd FileType markdown setlocal foldmethod=indent

autocmd TermOpen * setlocal nonumber norelativenumber

" indentation
set tabstop=4
set shiftwidth=4
set updatetime=100
set autoindent

" useful bindings
inoremap jk <ESC>
tnoremap jk <C-\><C-N> 
nnoremap jk :ls<CR>:buffer<space>

inoremap {<CR> {<CR><Tab><End><CR><BS>}<Up><Right>
nnoremap <Leader><Leader> :e#<CR>
nnoremap <Leader>f :F<CR>
nnoremap <Leader>/ :let @/ = ""<CR>

nnoremap <C-s> :w<CR>
nnoremap <tab> :tabnext<CR>
nnoremap <S-tab> :tabprevious<CR>
nnoremap <C-q> :bd<CR>
nnoremap !<C-q> :bd!<CR>

nnoremap 0 ^

command! -nargs=1 -complete=help H help <args> | silent only

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

