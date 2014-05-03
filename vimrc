set nocompatible              "not compatible with vi, improve performance
filetype off

" set the runtime path to include Vundle and initialize
set rtp+=~/dotfile/vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle, required
" general
Plugin 'gmarik/vundle'
Plugin 'Lokaltog/vim-easymotion'
Bundle 'kien/ctrlp.vim'
Bundle 'bitc/vim-bad-whitespace'
Bundle 'ciaranm/detectindent'
Bundle 'scrooloose/nerdcommenter'
Bundle 'scrooloose/nerdtree'
Bundle 'scrooloose/syntastic'
Bundle 'majutsushi/tagbar'
Bundle 'troydm/easybuffer.vim'
Bundle 'jnurmine/Zenburn'
Bundle 'Rip-Rip/clang_complete'
"Bundle 'mileszs/ack.vim'
Bundle 'Lokaltog/vim-powerline'
Bundle 'mattn/emmet-vim'
Bundle 'Gundo'
" lang specific
"Bundle "pangloss/vim-javascript"
"Bundle 'Valloric/YouCompleteMe'
"Bundle 'guns/vim-clojure-static'
"Bundle 'marijnh/tern_for_vim'
"Bundle 'wting/rust.vim'
"Bundle 'tpope/vim-liquid'
"Bundle 'tpope/vim-markdown'
"Bundle 'psykidellic/vim-jekyll'
"Bundle 'digitaltoad/vim-jade'

nnoremap <F3> :GundoToggle<CR>
nnoremap <F8> :TagbarToggle<CR>

nnoremap <F4> :NERDTreeToggle<CR>
let g:tagbar_autofocus = 1
let NERDTreeIgnore = ['\.pyc$', '\~$', '\.o$', '\.class$', '\.out$', '\.o$']
" close vim if the Nerdtree is the only window left
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

"Enable plugin and indenting plugins
filetype plugin indent on

"change the mapleader from \ to ,
let mapleader = ","
let g:mapleader = ","

nmap <leader>b :EasyBufferToggle<CR>

set history=666
set undolevels=1000

" Set to auto read when a file is changed from the outside
set autoread

" Ignore compiled files
set wildignore=*.o,*~,*.pyc

 "Fast saving
nmap <leader>w :w!<cr>

map <leader>/ <plug>NERDCommenterToggle

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

set shiftwidth=4
set tabstop=4
set softtabstop=4 "number of spaces in tab
set expandtab "tabs are spaces
set smarttab

set showcmd "show command in bottom bar

set wildmenu "ctrl+n and ctrl+p to go back and fro

set lazyredraw "redraw only when need to

set cc=120
set title

set hlsearch "highlight matches
nnoremap <leader><space> :nohlsearch<CR>
set ignorecase
set smartcase
set infercase
set showmatch "highlight matching [{()}]
set incsearch "search as characters are entered
set nowrap

"No .swap files and backup
set nobackup
set noswapfile
set nowb

set foldenable "enalbe folding
"set foldlevelstart=10 "enable it when necessary

nnoremap <space> za "space open/closes folds

" Detect Indent
let g:detectindent_preferred_expandtab = 1
let g:detectindent_preferred_indent = 4
autocmd BufNewFile,BufReadPost * :DetectIndent
autocmd FileType make setlocal noexpandtab

" Strip trailing ws
if !exists("*StripWS")
  function StripWS()
      :%s/\s\+$//e
  endfunction
endif

"Height of the command bar
set cmdheight=2

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Colors and Fonts
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Enable syntax highlighting
syntax enable
set background=dark
"let g:solarized_termcolors=256
"let g:solarized_termtrans=1
colorscheme solarized

" Treat long lines as break lines (useful when moving around in them)
map j gj
map k gk

"enable mouse
set mouse=a

nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>

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

set clipboard+=unnamedplus
