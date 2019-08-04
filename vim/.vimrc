"# -------------------------------------------------------------------
"# file:    ~/.vimrc
"# author:  WalkerGriggs    www.walkergriggs.com
"# -------------------------------------------------------------------
set nocompatible
filetype off

execute pathogen#infect()

" General Vim-Airline config
set laststatus=2
set encoding=utf-8
set term=screen-256color

" Use minimalist color scheme for syntax
set t_Co=256
syntax on
colorscheme minimalist

" Use minimalist color scheme for airline
let g:airline_theme='minimalist'
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1

" Don't use swapfile of backups
set noswapfile
set nobackup
set nowritebackup

" Setting airline symbols.
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif

" unicode symbols
let g:airline_left_sep = '»'
let g:airline_left_sep = '▶'
let g:airline_right_sep = '«'
let g:airline_right_sep = '◀'
let g:airline_symbols.crypt = '🔒'
let g:airline_symbols.linenr = '␊'
let g:airline_symbols.linenr = '␤'
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.maxlinenr = '☰'
let g:airline_symbols.maxlinenr = ''
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.paste = 'Þ'
let g:airline_symbols.paste = '∥'
let g:airline_symbols.spell = 'Ꞩ'
let g:airline_symbols.notexists = '∄'
let g:airline_symbols.whitespace = 'Ξ'

" I SHOULDN'T HAVE TO SPEND HOURS TIDDLING WITH FONTS
" TO GET COOL LITTLE SLANTY ARROWS.
" NOPE
" NO WAY AM I ABOUT TO DO THAT
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ''

" Removes the need for Shift-;
nnoremap ; :

" Splitting made easy...
set splitbelow
set splitright

" Toggle NERDTree
nnoremap <silent> <F5> :NERDTreeToggle<CR>
nnoremap <silent> <F6> :NERDTreeCWD<CR>

set wildmenu "better command line completion
set showcmd " Show partial commands
set mouse=a "enable the use of mouse

set ruler
set visualbell
set textwidth=80
set hidden

" Tabbing rules
set tabstop=2
set expandtab
set softtabstop=2
set shiftwidth=2

" Indentation rules
filetype indent on
set autoindent

" Handly litte tidbits
set number
set showcmd
set nocursorline
set showmatch

" Whitespace
set list lcs=eol:¬,tab:>-,trail:.,nbsp:_

filetype on
filetype indent on

" vim-go
let g:go_fmt_fail_silently = 0
let g:go_fmt_command = "goimports"
let g:go_autodetect_gopath = 1
let g:go_term_enabled = 1
let g:go_snippet_engine = "neosnippet"
let g:go_highlight_space_tab_error = 0
let g:go_highlight_array_whitespace_error = 0
let g:go_highlight_trailing_whitespace_error = 0
let g:go_highlight_extra_types = 0
let g:go_highlight_operators = 0
let g:go_highlight_build_constraints = 1
let g:go_fmt_autosave = 1

" Xterm title
let &term = $TERM
set titlestring=%t%(\ %M%)%(\ (%{expand(\"%:p:h\")})%)%(\ %a%)\ -\ %{v:servername}
if &term == "xterm" || &term == "xterm-color" || $TERM == "xterm-256color"
  set title
endif
