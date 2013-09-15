set nocompatible               " be iMproved
 filetype off                   " required!

set encoding=utf-8
set fileencoding=utf-8


 map <F2> :NERDTreeToggle<CR>
 " Powerline setup
 set guifont=DejaVu\ Sans\ Mono\ for\ Powerline\ 9
 set laststatus=2

 set rtp+=~/.vim/bundle/vundle/
 call vundle#rc()

 " let Vundle manage Vundle
 " required! 
 Bundle 'gmarik/vundle'
 Bundle 'scrooloose/nerdtree'
 Bundle 'klen/python-mode'
 Bundle 'Lokaltog/powerline', {'rtp': 'powerline/bindings/vim/'}

 "Javascript IDE
 Bundle "pangloss/vim-javascript"
 " My Bundles here:
 "
 " original repos on github
 Bundle 'tpope/vim-fugitive'
 Bundle 'Lokaltog/vim-easymotion'
 Bundle 'rstacruz/sparkup', {'rtp': 'vim/'}
 Bundle 'tpope/vim-rails.git'
 " vim-scripts repos
 Bundle 'L9'
 Bundle 'FuzzyFinder'
 " non github repos
 Bundle 'git://git.wincent.com/command-t.git'

 " git repos on your local machine (ie. when working on your own plugin)
 " Bundle 'file:///Users/gmarik/path/to/plugin'
 " ...

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
 "
 " Brief help
 " :BundleList          - list configured bundles
 " :BundleInstall(!)    - install(update) bundles
 " :BundleSearch(!) foo - search(or refresh cache first) for foo
 " :BundleClean(!)      - confirm(or auto-approve) removal of unused bundles
 "
 " see :h vundle for more details or wiki for FAQ
 " NOTE: comments after Bundle command are not allowed..
