"not compatible with vi, improve performance
set nocompatible               

"force reloading after pathogen loaded
filetype off

"enalbe pathogen
"execute pathogen#infect()
"call pathogen#incubate() 
"call pathogen#helptags()
"Customize vimrc
"Enable plugin and indenting plugins
filetype plugin indent on

"change the mapleader from \ to ,
let mapleader = ","
let g:mapleader = ","

"Sets how many lines of history vim has to remember
set history=666

" Set to auto read when a file is changed from the outside
set autoread

" Ignore compiled files
set wildignore=*.o,*~,*.pyc

" Fast saving
nmap <leader>w :w!<cr>

"Always show current position
set ruler

" No annoying sound on errors
set noerrorbells
set novisualbell
set t_vb=
set tm=500

"Set 7 lines to the cursor when moving vertically using j/k
set so=7

"show the line number
set nu

" Set utf8 as standard encoding and en_US as the standard language
set encoding=utf-8
set fileencoding=utf-8

"shift width
set shiftwidth=2

"Highlight search results
set hlsearch

"Makes search act like search in modern browsers
set incsearch

"No .swap files and backup
"use git
set nobackup
set noswapfile
set nowb

"Height of the command bar
set cmdheight=2

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Colors and Fonts
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Enable syntax highlighting
syntax enable

"background color
set background=dark

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Moving around, tabs, windows and buffers
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Treat long lines as break lines (useful when moving around in them)
map j gj
map k gk

"enable mouse
set mouse=a

nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>

"temp setup
"Disable some keys
map <up> <nop>
map <down> <nop>
map <left> <nop>
map <right> <nop>

"My personal mapping
inoremap <c-d> <esc>ddi
inoremap <c-l> <del>

nnoremap <c-h> <c-w>h
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-l> <c-w>l

"Toggle paste mode on and off
nnoremap <leader>pp :setlocal paste!<cr>

" Returns true if paste mode is enabled
function! HasPaste()
    if &paste
        return 'PASTE MODE  '
    en
    return ''
endfunction

" vimdiff keybinding
" get from local
nmap <silent> <leader>dl :diffget LO<cr>
" get from base
nmap <silent> <leader>db :diffget BA<cr>
" get from remote
nmap <silent> <leader>dr :diffget RE<cr>

set clipboard=unnamedplus
