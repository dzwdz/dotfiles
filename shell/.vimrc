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
