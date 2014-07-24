set nocompatible              "not compatible with vi, improve performance
filetype off

silent function! OSX()
    return has('macunix')
endfunction

silent function! LINUX()
    return has('unix') && !has('macunix') && !has('win32unix')
endfunction

silent function! WINDOWS()
    return (has('win32') || has('win64'))
endfunction

if !WINDOWS()
    set shell=/bin/sh
endif

" set the runtime path to include Vundle and initialize
if WINDOWS()
    set rtp+=~/vimfiles/bundle/Vundle.vim/
    let path='~/vimfiles/bundle'
    call vundle#begin(path)
else
    set rtp+=~/dotfile/vim/bundle/Vundle.vim
    call vundle#begin()
endif

if has("gui_running")
    autocmd GUIEnter * set vb t_vb=
endif

" Vundle manages vundle
Plugin 'gmarik/vundle'

if has('unix')
    Plugin 'Rip-Rip/clang_complete'
    Plugin 'taglist.vim'
    Plugin 'moll/vim-node'
    Plugin 'wookiehangover/jshint.vim'
endif

" general plugin
Plugin 'vim-scripts/SearchComplete'
Plugin 'ervandew/supertab'
Plugin 'vim-scripts/ShowMarks'
Plugin 'Lokaltog/vim-easymotion'
Plugin 'kien/ctrlp.vim'
Plugin 'bitc/vim-bad-whitespace'
Plugin 'ciaranm/detectindent'
Plugin 'scrooloose/nerdcommenter'
Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/syntastic'
Plugin 'majutsushi/tagbar'
Plugin 'troydm/easybuffer.vim'
Plugin 'jnurmine/Zenburn'
"Plugin 'mileszs/ack.vim'
Plugin 'Lokaltog/vim-powerline'
Plugin 'mattn/emmet-vim'
Plugin 'Gundo'
Plugin 'spf13/vim-autoclose'
Plugin 'tpope/vim-surround'
" lang specific
Plugin 'pangloss/vim-javascript'
Plugin 'jelera/vim-javascript-syntax'
Plugin 'digitaltoad/vim-jade'
Plugin 'wavded/vim-stylus'
Plugin 'kchmck/vim-coffee-script'
"Plugin 'Valloric/YouCompleteMe'
"Plugin 'guns/vim-clojure-static'
"Plugin 'marijnh/tern_for_vim'
"Plugin 'wting/rust.vim'
"Plugin 'tpope/vim-liquid'
"Plugin 'tpope/vim-markdown'
"Plugin 'psykidellic/vim-jekyll'

"Plugins related to vim-snipmate
Bundle "MarcWeber/vim-addon-mw-utils"
Bundle "tomtom/tlib_vim"
Bundle "garbas/vim-snipmate"
Bundle "honza/vim-snippets"

" All of your Plugins must be added before the following line
call vundle#end()
filetype plugin indent on
" Enable syntax highlighting
syntax enable

autocmd Filetype javascript setlocal ts=2 sts=2 sw=2

nnoremap <F2> :TagbarToggle<CR>
nnoremap <F3> :GundoToggle<CR>
nnoremap <F4> :NERDTreeToggle<CR>

" In vim, don't do auto close.
let g:autoclose_vim_commentmode = 1

let g:tagbar_usearrows = 1

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

"CtrlP settings
let g:ctrlp_match_window = 'bottom,order:ttb'
let g:ctrlp_switch_buffer = 0
let g:ctrlp_working_path_mode = ''
" to-do use external command to speed up ctrlp
" let g:ctrlp_user_command = 'ag %s -l --nocolor --hidden -g ""'

" Set to auto read when a file is changed from the outside
set autoread

" Ignore compiled files
set wildignore=*.o,*~,*.pyc,*.class

nmap <leader>w :w!<cr>

map <leader>/ <plug>NERDCommenterToggle

set ruler  "Always show current position
set showcmd "show command in bottom bar

if has('statusline')
    set laststatus=2
" Broken down into easily includeable segments
    set statusline=%<%f\ " Filename
    set statusline+=%w%h%m%r " Options
    "set statusline+=%{fugitive#statusline()} " Git Hotness
    set statusline+=\ [%{&ff}/%Y] " Filetype
    set statusline+=\ [%{getcwd()}] " Current dir
    set statusline+=%=%-14.(%l,%c%V%)\ %p%% " Right aligned file nav info
endif

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

" Formatting
set autoindent  " Indent at the same level of the previous line
set smartindent
set expandtab "tabs are spaces
set shiftwidth=4
set tabstop=4
set softtabstop=4 "number of spaces in tab
set smarttab
set splitright "put new windows to the right of the current window
set splitbelow "put new windows to the bottom of the current window

" show list instead of just completing
set wildmenu "ctrl+n and ctrl+p to go back and fro
set listchars=tab:›\ ,trail:•,extends:#,nbsp:. " Highlight problematic whitespace

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
set wrap


set winaltkeys=no

"No .swap files and backup
set nobackup
set noswapfile
set nowb

set foldenable "enalbe folding
set foldlevelstart=10 "enable it when necessary
set foldnestmax=10 "10 nested fold max
nnoremap <space> za "space open/closes folds
set foldmethod=indent   " fold based on indent level

" Detect Indent
let g:detectindent_preferred_expandtab = 1
let g:detectindent_preferred_indent = 4
"autocmd BufNewFile,BufReadPost * :DetectIndent
autocmd FileType make setlocal noexpandtab

"Height of the command bar
set cmdheight=2

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Colors and Fonts
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set background=dark
"let g:solarized_termcolors=256
"let g:solarized_termtrans=1
"let g:solarized_contrast="normal"
"let g:solarized_visibility="normal"
colorscheme solarized
set t_Co=256

" Treat long lines as break lines (useful when moving around in them)
map j gj
map k gk

set mouse=a  "enable mouse
set mousehide " Hide the mouse cursor while typing
scriptencoding utf-8

nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>

"Disable some keys
map <up> <nop>
map <down> <nop>
map <left> <nop>
map <right> <nop>


map <C-j> <C-w>j
map <C-h> <C-w>h
map <C-k> <C-w>k
map <C-l> <C-w>l

"My personal mapping
inoremap <c-d> <esc>ddi
inoremap <c-l> <del>

set winaltkeys=no


"Toggle paste mode on and off
nnoremap <leader>pp :setlocal paste!<cr>

" vimdiff keybinding
" get from local  (from the merging branch)
nmap <silent> <leader>gl :diffget LO<cr>
" get from base (from the common ancestor)
nmap <silent> <leader>gb :diffget BA<cr>
" get from remote (from the current branch)
nmap <silent> <leader>gr :diffget RE<cr>

"  allow the backspace key to erase previously entered characters, autoindent, and new lines
set backspace=indent,eol,start

set formatoptions-=o "dont continue comments when pushing o/O

set hidden "allow buffer switching without saving

set foldcolumn=1


" Return to last edit position when opening files (You want this!)
autocmd BufReadPost *
     \ if line("'\"") > 0 && line("'\"") <= line("$") |
     \   exe "normal! g`\"" |
     \ endif
" Remember info about open buffers on close
set viminfo^=%

" Delete trailing white space on save, useful for Python and CoffeeScript 
func! DeleteTrailingWS()
  exe "normal mz"
  %s/\s\+$//ge
  exe "normal `z"
endfunc
autocmd BufWrite *.py :call DeleteTrailingWS()
autocmd BufWrite *.coffee :call DeleteTrailingWS()

if has('clipboard')
    if has('unnamedplus') " When possible use + register for copy-paste
        set clipboard=unnamedplus
    else " On mac and Windows, use * register for copy-paste
        set clipboard=unnamed
    endif
endif

if has("cscope")
    " use both cscope and ctag for 'ctrl-]', ':ta', and 'vim -t'
    set cscopetag

    " check cscope for definition of a symbol before checking ctags: set to 1
    " if you want the reverse search order.
    set csto=0

    " add any cscope database in current directory
    if filereadable("cscope.out")
        cs add cscope.out
    " else add the database pointed to by environment variable
    elseif $CSCOPE_DB != ""
        cs add $CSCOPE_DB
    endif

    " show msg when any other cscope db added
    set cscopeverbose

    " The following maps all invoke one of the following cscope search types:
    "
    "   's'   symbol: find all references to the token under cursor
    "   'g'   global: find global definition(s) of the token under cursor
    "   'c'   calls:  find all calls to the function name under cursor
    "   't'   text:   find all instances of the text under cursor
    "   'e'   egrep:  egrep search for the word under cursor
    "   'f'   file:   open the filename under cursor
    "   'i'   includes: find files that include the filename under cursor
    "   'd'   called: find functions that function under cursor calls
    "
    " (Note: you may wish to put a 'set splitright' in your .vimrc
    " if you prefer the new window on the right instead of the left

    nmap <C-@>s :vert scs find s <C-R>=expand("<cword>")<CR><CR>
    nmap <C-@>g :vert scs find g <C-R>=expand("<cword>")<CR><CR>
    nmap <C-@>c :vert scs find c <C-R>=expand("<cword>")<CR><CR>
    nmap <C-@>t :vert scs find t <C-R>=expand("<cword>")<CR><CR>
    nmap <C-@>e :vert scs find e <C-R>=expand("<cword>")<CR><CR>
    nmap <C-@>f :vert scs find f <C-R>=expand("<cfile>")<CR><CR>
    nmap <C-@>i :vert scs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
    nmap <C-@>d :vert scs find d <C-R>=expand("<cword>")<CR><CR>

    "set notimeout
    " Or, you can keep timeouts, by uncommenting the timeoutlen line below,
    " with your own personal favorite value (in milliseconds):
    "
    "set timeoutlen=4000
    "set ttimeout
    "
    " Either way, since mapping timeout settings by default also set the
    " timeouts for multicharacter 'keys codes' (like <F1>), you should also
    " set ttimeout and ttimeoutlen: otherwise, you will experience strange
    " delays as vim waits for a keystroke after you hit ESC (it will be
    " waiting to see if the ESC is actually part of a key code like <F1>).
endif

" Mappings for ShowMarks
"\mt : Toggles ShowMarks on and off.
"\mh : Hides an individual mark.
"\ma : Hides all marks in the current buffer.
"\mm : Places the next available mark.

nnoremap <leader>sp :set spell!<CR>

if has('unix')
  " enable english dictionary
  set dictionary-=/usr/share/dict/words dictionary+=/usr/share/dict/words
  inoremap <C-f> <C-x><C-k>
endif
