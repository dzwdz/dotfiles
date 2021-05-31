" vim: foldmethod=marker : foldlevel=0

" Miscellaneous {{{
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

set path+=**

set colorcolumn=80

hi TabLineFill cterm=bold ctermbg=None
hi TabLine ctermfg=7 ctermbg=None cterm=none
hi TabLineSel ctermfg=3
hi LineNr ctermfg=7
hi ColorColumn ctermbg=black

hi VertSplit cterm=none
hi StatusLine cterm=bold
hi StatusLineNC cterm=bold

hi PMenu ctermbg=102

if has('nvim')
	autocmd TermOpen * setlocal nonumber norelativenumber
endif
autocmd BufWritePost ~/.vimrc source ~/.vimrc
" }}}
" Indentation / Folding {{{
set tabstop=4
set shiftwidth=4
set updatetime=100
set autoindent

autocmd FileType ruby setlocal shiftwidth=2 tabstop=2 expandtab
autocmd FileType haskell setlocal shiftwidth=2 tabstop=2 expandtab
autocmd FileType txt setlocal foldmethod=indent
autocmd FileType markdown setlocal foldmethod=indent

set foldmethod=syntax
set foldlevel=99
hi Folded ctermbg=NONE

set list
set listchars=tab:·\ 

set fillchars=fold:\ 
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

nmap <silent> <c-k> :wincmd k<CR>
nmap <silent> <c-j> :wincmd j<CR>
nmap <silent> <c-h> :wincmd h<CR>
nmap <silent> <c-l> :wincmd l<CR>

nnoremap 0 ^

command! -nargs=1 -complete=help H help <args> | silent only

" gets rid of all buffers expect unsaved ones and the current one
command! Rid silent! execute "%bd\|e#"

" make the current file executable
command! MakeX execute "!chmod +x \"%:p\""

iab rubysb #!/usr/bin/env ruby
iab vifold vim: foldmethod=marker : foldlevel=0
iab <expr> isonow strftime("%FT%T%z")
""" }}}
" Plugin specific {{{
hi GitGutterAdd		ctermbg=NONE ctermfg=GREEN
hi GitGutterChange	ctermbg=NONE ctermfg=YELLOW
hi GitGutterDelete	ctermbg=NONE ctermfg=RED
hi SignColumn		ctermbg=NONE
let g:gitgutter_sign_modified = '~'

set rtp+=/usr/bin/fzf
"let g:fzf_layout = { 'window': 'enew' }

runtime ftplugin/man.vim

let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_winsize = 20

let g:vimwiki_list = [{'path': '~/Notes/',
                      \ 'path_html': '~/Notes_html'}]
" }}}
