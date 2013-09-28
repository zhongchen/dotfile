set nocompatible               " be iMproved
filetype off                   " required!

set nu
set encoding=utf-8
set fileencoding=utf-8
set shiftwidth=2
set hlsearch
set incsearch

let mapleader=","

nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>

 map <F2> :NERDTreeToggle<CR>
 " Powerline setup
 set guifont=DejaVu\ Sans\ Mono\ for\ Powerline\ 9
 set laststatus=2


 augroup vimrc_autocmds
	     autocmd!
	         " highlight characters past column 120
	     autocmd FileType python highlight Excessctermbg=DarkGrey guibg=Black
     	     autocmd FileType python match Excess /\%80v.*/
	     autocmd FileType python set nowrap
augroup END
 
 execute pathogen#infect()
 syntax on
 filetype plugin indent on     " required!
