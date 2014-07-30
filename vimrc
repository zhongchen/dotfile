"not compatible with vi, improve performance
set nocompatible
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

" Windows Compatible {
" On Windows, also use '.vim' instead of 'vimfiles'; this makes synchronization across (heterogeneous) systems easier.
if WINDOWS()
  set runtimepath=$HOME/.vim,$VIM/vimfiles,$VIMRUNTIME
  " Be nice and check for multi_byte even if the config requires
  " multi_byte support most of the time
  if has("multi_byte")
    " Windows cmd.exe still uses cp850. If Windows ever moved to
    " Powershell as the primary terminal, this would be utf-8
    set termencoding=cp850
    " Let Vim use utf-8 internally, because many scripts require this
    set encoding=utf-8
    setglobal fileencoding=utf-8
    " Windows has traditionally used cp1252, so it's probably wise to
    " fallback into cp1252 instead of eg. iso-8859-15.
    " Newer Windows files might contain utf-8 or utf-16 LE so we might
    " want to try them first.
    set fileencodings=ucs-bom,utf-8,utf-16le,cp1252,iso-8859-15
  endif
endif
" }

if !WINDOWS()
    let $BASH_ENV="~/.bash_profile"
    set shell=/bin/bash
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

let g:bundle_groups=['general','neocomplete', 'programming', 'python', 'javascript', 'misc',]

" Vundle manages vundle
Plugin 'gmarik/vundle'
Plugin 'MarcWeber/vim-addon-mw-utils' "utility library
Plugin 'tomtom/tlib_vim' "utility library
if executable('ag')
    Plugin 'mileszs/ack.vim'
    let g:ackprg = 'ag --nogroup --nocolor --column --smart-case'
elseif executable('ack-grep')
    let g:ackprg="ack-grep -H --nocolor --nogroup --column"
    Plugin 'mileszs/ack.vim'
elseif executable('ack')
    Plugin 'mileszs/ack.vim'
endif

" general plugin {
if count(g:bundle_groups, 'general')
    Plugin 'ervandew/supertab'
    Plugin 'kien/ctrlp.vim'
    Plugin 'Lokaltog/vim-easymotion'
    Plugin 'scrooloose/nerdtree'
    Plugin 'troydm/easybuffer.vim' "quickly switch between buffers.
    Plugin 'scrooloose/nerdcommenter'
    Plugin 'scrooloose/syntastic'
    Plugin 'spf13/vim-autoclose'
    Plugin 'tpope/vim-surround' "change surrounding.
    Plugin 'tpope/vim-repeat'
    Plugin 'mbbill/undotree'
    Plugin 'terryma/vim-multiple-cursors'
    Plugin 'jistr/vim-nerdtree-tabs'
    Plugin 'godlygeek/csapprox' "make colorscheme work in terminal
    Plugin 'nathanaelkane/vim-indent-guides' "visually displaying indent levels
    Plugin 'taglist.vim'
    Plugin 'godlygeek/tabular'
    if has('python') || has('python3')
        Plugin 'Lokaltog/powerline', {'rtp': 'powerline/bindings/vim/'}
    endif
    if executable('ctags')
        Plugin 'majutsushi/tagbar'
    endif
endif
"}

if count(g:bundle_groups, 'misc')
    Plugin 'tpope/vim-markdown'
    Plugin 'nelstrom/vim-markdown-folding'
    if has('ruby')
        Plugin 'greyblake/vim-preview' "<leader>P
    endif
    Plugin 'exu/pgsql.vim' "postgresql syntax
    Plugin 'vim-scripts/ShowMarks'
    Plugin 'bitc/vim-bad-whitespace' "highlight the ending white space.
    Plugin 'ciaranm/detectindent'
endif

" Snippets & autocomplete {
if count(g:bundle_groups, 'neocomplete')
    Plugin 'Shougo/neocomplete.vim.git'
    Plugin 'Shougo/neosnippet'
    Plugin 'Shougo/neosnippet-snippets'
    Plugin 'honza/vim-snippets'
endif
let g:neocomplete#enable_at_startup = 1
" }

Plugin 'mattn/emmet-vim'

" lang specific
" Javascript {
if count(g:bundle_groups, 'javascript')
    Plugin 'elzr/vim-json'
    Plugin 'pangloss/vim-javascript'
    Plugin 'jelera/vim-javascript-syntax'
    Plugin 'digitaltoad/vim-jade'
    Plugin 'moll/vim-node'
    Plugin 'kchmck/vim-coffee-script'
    Plugin 'wookiehangover/jshint.vim'
    Plugin 'wavded/vim-stylus'
    Plugin 'marijnh/tern_for_vim' "provides Tern-based JavaScript editing
endif
"}

" Python {
if count(g:bundle_groups, 'python')
    Plugin 'klen/python-mode'
    Plugin 'python.vim'
    Plugin 'python_match.vim'
    Plugin 'pythoncomplete'
endif
" }



" All of your Plugins must be added before the following line
call vundle#end()

filetype plugin indent on
" Enable syntax highlighting
syntax enable

autocmd Filetype javascript setlocal ts=2 sts=2 sw=2

nnoremap <F3> :UndotreeToggle<CR>
if has("persistent_undo")
    set undodir='~/.undodir/'
    set undofile
endif

" In vim, don't do auto close.
let g:autoclose_vim_commentmode = 1

nnoremap <F2> :TagbarToggle<CR>
let g:tagbar_usearrows = 1
let g:tagbar_autofocus = 1

nnoremap <F4> :NERDTreeToggle<CR>
let NERDTreeIgnore = ['\.pyc$', '\~$', '\.o$', '\.class$', '\.out$', '\.o$']
" close vim if the Nerdtree is the only window left
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif


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

nmap <leader>w :w!<cr>
map <leader>/ <plug>NERDCommenterToggle

set ruler  "Always show current position
set showcmd "show command in bottom bar

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
set wildmode=list:longest,full

" Ignore compiled files
set wildignore+=*.class
set wildignore+=.hg,.git,.svn                    " Version control
set wildignore+=*.aux,*.out,*.toc                " LaTeX intermediate files
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg   " binary images
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest " compiled object files
set wildignore+=*.spl                            " compiled spelling word lists
set wildignore+=*.DS_Store                       " OSX bullshit
set wildignore+=*.pyc                            " Python byte code
set wildignore+=*.orig                           " Merge resolution files

" Highlight problematic whitespace
set listchars=tab:›\ ,trail:•,extends:#,nbsp:.


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

" folding related.
set foldenable "enalbe folding
set foldlevelstart=10 "enable it when necessary
set foldnestmax=10 "10 nested fold max
nnoremap <space> za "space open/closes folds
vnoremap <space> za
set foldmethod=expr   " fold based on indent level

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
let g:solarized_termcolors=256
let g:solarized_termtrans=1
let g:solarized_contrast="normal"
let g:solarized_visibility="normal"
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
inoremap <c-d> <del>

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
"<leader>mt : Toggles ShowMarks on and off.
"<leader>mh : Hides an individual mark.
"<leader>ma : Hides all marks in the current buffer.
"<leader>mm : Places the next available mark.

nnoremap <leader>sp :set spell!<CR>

" Visual shifting (does not exit Visual mode)
vnoremap < <gv
vnoremap > >gv

" Allow using the repeat operator with a visual selection (!)
" http://stackoverflow.com/a/8064607/127816
vnoremap . :normal .<CR>

" For when you forget to sudo.. Really Write the file.
cmap w!! w !sudo tee % >/dev/null

" Select (charwise) the contents of the current line, excluding indentation.
" " Great for pasting Python lines into REPLs.
nnoremap vv ^vg_

" mpping for fast opening files.
map <leader>ew :e <C-R>=expand("%:p:h") . "/" <CR>
map <leader>es :vsp <C-R>=expand("%:p:h") . "/" <CR>
map <leader>et :tabe <C-R>=expand("%:p:h") . "/" <CR>

if has('unix')
  " enable english dictionary
  set dictionary-=/usr/share/dict/words dictionary+=/usr/share/dict/words
  inoremap <C-f> <C-x><C-k>
endif
