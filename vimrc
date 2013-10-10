"Customize vimrc
"Enable filetype plugins
filetype plugin on     " required!
filetype indent on     " required!

"enalbe pathogen
"execute pathogen#infect()

"Sets how many lines of history vim has to remember
set history=666

" Set to auto read when a file is changed from the outside
set autoread

" With a map leader it's possible to do extra key combinations
" like <leader>w saves the current file
let mapleader = ","
let g:mapleader = ","

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

set nocompatible               " be iMproved

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




nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>

 map <F2> :NERDTreeToggle<CR>


"temp setup
"Disable some keys
map <up> <nop>
map <down> <nop>
map <left> <nop>
map <right> <nop>
