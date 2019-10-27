# Path --------------------------------------------------------
export PATH=$HOME/bin:/usr/local/bin:$PATH

export PATH=$PATH:/usr/local/go/bin:$GOPATH/bin

export PATH=$PATH:$HOME/Builds/bin:$HOME/.bin

export PATH=$PATH:$HOME/.rvm/bin

# General -----------------------------------------------------
export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="spaceship"

# Keyboard Layout -------------
alias ANSI='setxkbmap us'
alias JIS='setxkbmap jp'
alias XMM_STD='setxkbmap us && xmodmap ~/.Xmodmap_std'
alias XMM_WKL='setxkbmap us && xmodmap ~/.Xmodmap_wkl'

# Docker ----------------------
alias dps='docker ps -a'
alias drm_stopped='docker ps -aq --no-trunc | xargs docker rm'
alias drmi_untagged='docker images -q --filter dangling=true | xargs docker rmi'
alias dbuild='docker build --rm -t'

# Golang ----------------------
export GOPATH=$HOME/go

# AWS -------------------------
export AWS_PROFILE=wgriggs

# General Utils ---------------
alias reload='xrdb $HOME/.Xresources & source $HOME/.zshrc'
alias wifi='nmtui'
alias news='newsbeuter -r -C $HOME/.config/newsbeuter/config -u $HOME/.config/newsbeuter/urls'
alias modes='stat -c "%a %n" *'

# List ------------------------
alias l='exa -uahg --group-directories-first'
alias ll='l -l --git'
alias tree='exa -lhaT --git --group-directories-first'

# ZSH ---------------------------------------------------------
plugins=(git zsh-autosuggestions)

source $ZSH/oh-my-zsh.sh

export UPDATE_ZSH_DAYS=14
export LANG=en_US.UTF-8

DISABLE_AUTO_UPDATE="true"
DISABLE_AUTO_TITLE="true"

# Spaceship Prompt --------------------------------------------
SPACESHIP_PROMPT_ORDER=(
  time          # Time stamps section
  user          # Username section
  dir           # Current directory section
  host          # Hostname section
  git           # Git section (git_branch + git_status)
  hg            # Mercurial section (hg_branch  + hg_status)
  package       # Package version
  node          # Node.js section
  ruby          # Ruby section
  elixir        # Elixir section
  xcode         # Xcode section
  swift         # Swift section
  golang        # Go section
  php           # PHP section
  rust          # Rust section
  haskell       # Haskell Stack section
  julia         # Julia section
  docker        # Docker section
  # aws           # Amazon Web Services section
  venv          # virtualenv section
  conda         # conda virtualenv section
  pyenv         # Pyenv section
  dotnet        # .NET section
  ember         # Ember.js section
  # kubecontext # Kubectl context section
  terraform     # Terraform workspace section
  exec_time     # Execution time
  line_sep      # Line break
  battery       # Battery level and status
  vi_mode       # Vi-mode indicator
  jobs          # Background jobs indicator
  exit_code     # Exit code section
  char          # Prompt character
)

SPACESHIP_TIME_SHOW=true
