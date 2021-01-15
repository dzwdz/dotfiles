inoremap jk <ESC>
let mapleader = "'"

syntax on
set number
set noswapfile
set hlsearch
set ignorecase
set incsearch

set spell spelllang=en_us

hi GitGutterAdd		ctermbg=black
hi GitGutterChange	ctermbg=black
hi GitGutterDelete	ctermbg=black
hi SignColumn		ctermbg=black

let g:gitgutter_sign_modified = '~'

set tabstop=4
set updatetime=100
