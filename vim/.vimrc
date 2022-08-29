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

" Don't use swapfile of backups
set noswapfile
set nobackup
set nowritebackup

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

" Xterm title
let &term = $TERM
set titlestring=%t%(\ %M%)%(\ (%{expand(\"%:p:h\")})%)%(\ %a%)\ -\ %{v:servername}
if &term == "xterm" || &term == "xterm-color" || $TERM == "xterm-256color"
  set title
endif
