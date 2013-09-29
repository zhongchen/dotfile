"Customize vimrc
filetype plugin on     " required!
filetype indent on                   " required!
execute pathogen#infect()
syntax on

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

"Always show current position
set ruler

" No annoying sound on errors
set noerrorbells
set novisualbell
set t_vb=
set tm=500

set nocompatible               " be iMproved

"show the line number
set nu

set encoding=utf-8
set fileencoding=utf-8

"shift width
set shiftwidth=2

"Highlight search results
set hlsearch

"Makes search act like search in modern browsers
set incsearch

"No .swap files and backup
set nobackup
set noswapfile


nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>

 map <F2> :NERDTreeToggle<CR>


 
