inoremap jk <ESC>
let mapleader = "'"

syntax on
set number
set noswapfile
set hlsearch
set ignorecase
set incsearch

set spell spelllang=en_us

hi GitGutterAdd		ctermbg=0
hi GitGutterChange	ctermbg=0
hi GitGutterDelete	ctermbg=0
hi SignColumn		ctermbg=0

let g:gitgutter_sign_modified = '~'

set tabstop=4
set updatetime=100

set autoindent

inoremap {<CR> {<CR><Tab><End><CR><BS>}<Up><Right>

set mouse=a
