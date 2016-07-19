set nocompatible "not compatible with vi, improve performance
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
    set rtp+=~/.vim/bundle/Vundle.vim
    call vundle#begin()
endif

let g:bundle_groups=['general','neocomplete', 'programming', 'python', 'javascript', 'misc',]

" Vundle manages vundle
Plugin 'gmarik/Vundle.vim'
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
    Plugin 'tpope/vim-sensible'
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
    Plugin 'tpope/vim-abolish'
    Plugin 'PProvost/vim-ps1'
    Plugin 'mattn/emmet-vim' "html plugin
    Plugin 'derekwyatt/vim-scala'
    Plugin 'fatih/vim-go'
    " Enable just for html/css
    let g:user_emmet_install_global = 0
    autocmd FileType html,css EmmetInstall
    if has('python') || has('python3')
        Plugin 'Lokaltog/powerline', {'rtp': 'powerline/bindings/vim/'}
        " Powerline setup
        set guifont=DejaVu\ Sans\ Mono\ for\ Powerline\ 9
        set laststatus=2
    endif
    if executable('ctags')
        Plugin 'majutsushi/tagbar'
    endif
endif
"}

if count(g:bundle_groups, 'misc')
    Plugin 'tpope/vim-markdown'
    Plugin 'nelstrom/vim-markdown-folding'
    Plugin 'JamshedVesuna/vim-markdown-preview'
    if has('ruby')
        Plugin 'greyblake/vim-preview' "<leader>P
    endif
    Plugin 'exu/pgsql.vim' "postgresql syntax
    "Plugin 'vim-scripts/ShowMarks'
    Plugin 'bitc/vim-bad-whitespace' "highlight the ending white space.
    Plugin 'ciaranm/detectindent'
endif

" Snippets & autocomplete {
if count(g:bundle_groups, 'neocomplete') && has('lua')
    Plugin 'Shougo/neocomplete.vim.git'
    Plugin 'Shougo/neosnippet'
    Plugin 'Shougo/neosnippet-snippets'
    Plugin 'honza/vim-snippets'

    " neocomplete configuration
    let g:neocomplete#enable_at_startup = 1
    let g:neocomplete#enable_smart_case = 1
    " Set minimum syntax keyword length.
    let g:neocomplete#sources#syntax#min_keyword_length = 3
    let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'
    " Define keyword.
     if !exists('g:neocomplete#keyword_patterns')
         let g:neocomplete#keyword_patterns = {}
     endif
    let g:neocomplete#keyword_patterns['default'] = '\h\w*'
    autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
    autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
    autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
    autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
     " Enable heavy omni completion.
    if !exists('g:neocomplete#sources#omni#input_patterns')
        let g:neocomplete#sources#omni#input_patterns = {}
    endif
    let g:neocomplete#sources#omni#input_patterns.c = '[^.[:digit:] *\t]\%(\.\|->\)'
    let g:neocomplete#sources#omni#input_patterns.cpp = '[^.[:digit:] *\t]\%(\.\|->\)\|\h\w*::'
    let g:neocomplete#sources#omni#input_patterns.javascript = '[^. \t]\.\w*'
endif
" }

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
    let g:tern_show_argument_hints='on_hold'
    let g:tern_map_keys=1
endif
"}

" Python {
if count(g:bundle_groups, 'python')
    Plugin 'klen/python-mode'
    Plugin 'python.vim'
    Plugin 'python_match.vim'
    Plugin 'pythoncomplete'
    Plugin 'davidhalter/jedi-vim'
    let g:jedi#goto_command = "<leader>d"
    let g:jedi#goto_assignments_command = "<leader>g"
    let g:jedi#goto_definitions_command = ""
    let g:jedi#documentation_command = "K"
    let g:jedi#usages_command = "<leader>n"
    let g:jedi#completions_command = "<C-Space>"
    let g:jedi#rename_command = "<leader>r"
endif
" }

" PyMode {
" Disable if python support not present
if !has('python')
    let g:pymode = 0
endif

if isdirectory(expand("~/.vim/bundle/python-mode"))
    let g:pymode_lint_checkers = ['pyflakes']
    let g:pymode_trim_whitespaces = 0
    let g:pymode_options = 0
    let g:pymode_rope = 0
" K             Show python docs
" [[            Jump on previous class or function (normal, visual, operator modes)
" ]]            Jump on next class or function (normal, visual, operator modes)
" [M            Jump on previous class or method (normal, visual, operator modes)
" ]M            Jump on next class or method (normal, visual, operator modes)
    " Documentation
    let g:pymode_doc = 1
    let g:pymode_doc_key = 'K'

    "Linting
    let g:pymode_lint = 1
    let g:pymode_lint_checker = "pyflakes,pep8"
    " Auto check on save
    let g:pymode_lint_write = 1

    " Support virtualenv
    let g:pymode_virtualenv = 1

    " Enable breakpoints plugin
    let g:pymode_breakpoint = 1
    let g:pymode_breakpoint_bind = '<leader>b'

    " syntax highlighting
    let g:pymode_syntax = 1
    let g:pymode_syntax_all = 1
    let g:pymode_syntax_indent_errors = g:pymode_syntax_all
    let g:pymode_syntax_space_errors = g:pymode_syntax_all

    " Override go-to.definition key shortcut to Ctrl-]
    let g:pymode_rope_goto_definition_bind = "<C-]>"

    " Override run current python file key shortcut
    let g:pymode_run_bind = "<leader>r"

    " Override view python doc key shortcut
    "let g:pymode_doc_bind = 

    " Don't autofold code
    let g:pymode_folding = 0
endif
" }

" All of your Plugins must be added before the following line
call vundle#end()

filetype plugin indent on
syntax enable " Enable syntax highlighting

if has('autocmd')
    " Return to last edit position when opening files (You want this!)
    autocmd BufReadPost *
         \ if line("'\"") > 0 && line("'\"") <= line("$") |
         \   exe "normal! g`\"" |
         \ endif

    autocmd Filetype javascript setlocal ts=2 sts=2 sw=2
    autocmd Filetype python setlocal ts=2 sts=2 sw=2
    autocmd BufNewFile,BufReadPost * :DetectIndent
    autocmd FileType make setlocal noexpandtab
    autocmd BufWritePre *.py,*.js :call DeleteTrailingWS()

    " Disable keybinding in plugins.
    autocmd VimEnter * unmap ]c

    " Disable keybinding in certain filetype.
    " autocmd Filetype python unmap! ]c
endif

if has("persistent_undo")
    set undodir='~/.undodir/'
    set undofile
endif

" In vim, don't do auto close.
let g:autoclose_vim_commentmode = 1

let mapleader = ","
let g:mapleader = ","

set history=1000
set undolevels=1000

"CtrlP settings
let g:ctrlp_match_window = 'bottom,order:ttb'
let g:ctrlp_switch_buffer = 0
let g:ctrlp_working_path_mode = ''
" to-do use external command to speed up ctrlp
" let g:ctrlp_user_command = 'ag %s -l --nocolor --hidden -g ""'

set autoread "Reload files changed outside vim.
set ruler  "Always show current position
set showcmd "show command in bottom bar

" No annoying sound on errors
set noerrorbells
set novisualbell
set t_vb=
set tm=500
" Disable error bell in gvim
autocmd GUIEnter * set vb t_vb=

set nrformats= " treat all numbers as decimals.

" make regexes more friendly
nnoremap / /\v
vnoremap / /\v

set so=7 "Set 7 lines to the cursor when moving vertically using j/k
set nu "show the line number.

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
let g:detectindent_preferred_expandtab = 4
let g:detectindent_preferred_indent = 4

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
" Remember info about open buffers on close
set viminfo^=%

" Delete trailing white space on save, useful for Python and CoffeeScript
func! DeleteTrailingWS()
  exe "normal mz"
  %s/\s\+$//ge
  exe "normal `z"
endfunc

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

"Google c++ coding sytle
" Detect if the current file type is a C-like language.
au BufNewFile,BufRead c,cpp,objc,*.mm call SetupForCLang()

" Configuration for C-like languages.
function! SetupForCLang()
    " Highlight lines longer than 80 characters.
    "au BufWinEnter * let w:m2=matchadd('ErrorMsg', '\%>80v.\+', -1)
    " Alternately, uncomment these lines to wrap at 80 characters.
    " setlocal textwidth=80
    " setlocal wrap

    " Use 2 spaces for indentation.
    setlocal shiftwidth=4
    setlocal tabstop=4
    setlocal softtabstop=4
    setlocal expandtab

    " Configure auto-indentation formatting.
    setlocal cindent
    setlocal cinoptions=h1,l1,g1,t0,i4,+4,(0,w1,W4
    setlocal indentexpr=GoogleCppIndent()
    let b:undo_indent = "setl sw< ts< sts< et< tw< wrap< cin< cino< inde<"

    " Uncomment these lines to map F5 to the CEF style checker. Change the path to match your system.
    " map! <F5> <Esc>:!python ~/code/chromium/src/cef/tools/check_style.py %:p 2> lint.out<CR>:cfile lint.out<CR>:silent !rm lint.out<CR>:redraw!<CR>:cc<CR>
    " map  <F5> <Esc>:!python ~/code/chromium/src/cef/tools/check_style.py %:p 2> lint.out<CR>:cfile lint.out<CR>:silent !rm lint.out<CR>:redraw!<CR>:cc<CR>
endfunction

" From https://github.com/vim-scripts/google.vim/blob/master/indent/google.vim
function! GoogleCppIndent()
    let l:cline_num = line('.')
    let l:orig_indent = cindent(l:cline_num)

    if l:orig_indent == 0 | return 0 | endif

    let l:pline_num = prevnonblank(l:cline_num - 1)
    let l:pline = getline(l:pline_num)
    if l:pline =~# '^\s*template' | return l:pline_indent | endif

    " TODO: I don't know to correct it:
    " namespace test {
    " void
    " ....<-- invalid cindent pos
    "
    " void test() {
    " }
    "
    " void
    " <-- cindent pos
    if l:orig_indent != &shiftwidth | return l:orig_indent | endif

    let l:in_comment = 0
    let l:pline_num = prevnonblank(l:cline_num - 1)
    while l:pline_num > -1
        let l:pline = getline(l:pline_num)
        let l:pline_indent = indent(l:pline_num)

        if l:in_comment == 0 && l:pline =~ '^.\{-}\(/\*.\{-}\)\@<!\*/'
            let l:in_comment = 1
        elseif l:in_comment == 1
            if l:pline =~ '/\*\(.\{-}\*/\)\@!'
                let l:in_comment = 0
            endif
        elseif l:pline_indent == 0
            if l:pline !~# '\(#define\)\|\(^\s*//\)\|\(^\s*{\)'
                if l:pline =~# '^\s*namespace.*'
                    return 0
                else
                    return l:orig_indent
                endif
            elseif l:pline =~# '\\$'
                return l:orig_indent
            endif
        else
            return l:orig_indent
        endif

        let l:pline_num = prevnonblank(l:pline_num - 1)
    endwhile

    return l:orig_indent
endfunction

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

nnoremap <F2> :TagbarToggle<CR>
let g:tagbar_usearrows = 1
let g:tagbar_autofocus = 1

nnoremap <F3> :UndotreeToggle<CR>

nnoremap <F4> :NERDTreeToggle<CR>
let NERDTreeIgnore = ['\.pyc$', '\~$', '\.o$', '\.class$', '\.out$', '\.o$']
" close vim if the Nerdtree is the only window left
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

nnoremap <f6> :!ctags -R<CR>
nnoremap <leader>l :EasyBufferToggle<CR>
nmap <leader>w :w!<cr>
map <leader>/ <plug>NERDCommenterToggle
nnoremap <leader><space> :nohlsearch<CR>

set autowrite

" quickfix window
map <leader>en :cnext <CR>
map <leader>ep :cprevious <CR>
map <leader>ec :cclose <CR>

" Vim Go mapping
" https://github.com/fatih/vim-go-tutorial
autocmd FileType go nmap <leader>gr  <Plug>(go-run)
function! s:build_go_files()
  let l:file = expand('%')
  if l:file =~# '^\f\+_test\.go$'
    call go#cmd#Test(0, 1)
  elseif l:file =~# '^\f\+\.go$'
    call go#cmd#Build(0)
  endif
endfunction
autocmd FileType go nmap <leader>gb :<C-u>call <SID>build_go_files()<CR>
autocmd FileType go nmap <Leader>gd <Plug>(go-doc)
autocmd FileType go nmap <Leader>gt <Plug>(go-test)
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_fields = 1
let g:go_highlight_types = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1
let g:go_fmt_command = "goimports"
