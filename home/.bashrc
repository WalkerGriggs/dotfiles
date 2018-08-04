#    ██████╗  █████╗ ███████╗██╗  ██╗██████╗  ██████╗
#    ██╔══██╗██╔══██╗██╔════╝██║  ██║██╔══██╗██╔════╝
#    ██████╔╝███████║███████╗███████║██████╔╝██║
#    ██╔══██╗██╔══██║╚════██║██╔══██║██╔══██╗██║
# ██╗██████╔╝██║  ██║███████║██║  ██║██║  ██║╚██████╗
# ╚═╝╚═════╝ ╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝╚═╝  ╚═╝ ╚═════╝
#
# Walker Griggs
# walker@walkergriggs.com
# github.com/WalkerGriggs

set -o noclobber # dont overwrite files

# Aliases ------------------------------------------------------

# Git --------------------------
if [ -f /usr/share/bash-completion/completions/git ]; then
    . /usr/share/bash-completion/completions/git
fi

for al in `__git_aliases`; do
    alias g$al="git $al"
done

# Keyboard Layout -------------
alias ANSI='setxkbmap us'
alias JIS='setxkbmap jp'
alias XMM_STD='setxkbmap us && xmodmap ~/.Xmodmap_std'
alias XMM_WKL='setxkbmap us && xmodmap ~/.Xmodmap_wkl'

# Cross Compiler --------------
export PREFIX="$HOME/opt/cross"
export TARGET=i686-elf
export PATH="$PREFIX/bin:$PATH"

export FONTCONFIG_PATH=/etc/fonts

# Docker ----------------------
alias dps='docker ps -a'
alias drm_stopped='docker ps -aq --no-trunc | xargs docker rm'
alias drmi_untagged='docker images -q --filter dangling=true | xargs docker rmi'
alias dbuild='docker build --rm -t'

# Golang ----------------------
export GOPATH=$HOME/go
export GIT_TERMINAL_PROMPT=1

# AWS -------------------------
export AWS_PROFILE=wgriggs

# General Utils ---------------
alias reload='xrdb $HOME/.Xresources & source $HOME/.bashrc & i3-msg restart'
alias wifi='nmtui'
alias news='newsbeuter -r -C $HOME/.config/newsbeuter/config -u $HOME/.config/newsbeuter/urls'
alias date='date "+%F %T"'
alias cal='ncal'
alias modes='stat -c "%a %n" *'
alias tmux='TERM=xterm-256color tmux'

# List ------------------------
alias l='exa -uahg --group-directories-first'
alias ll='l -l --git'
alias tree='exa -lhaT --git --group-directories-first'

alias ..='cd ../'

# System ----------------------
alias reboot="sudo shutdown -r now"
alias shutdown="sudo shutdown -h now"


# Path ---------------------------------------------------------

# Add gcc cross compiler for os dev
# export PATH="$PATH:$HOME/Builds/bin:$HOME/opt/cross/bin"

# Add Golang
export PATH="$PATH:/usr/local/go/bin:$GOPATH/bin"

# Add Scripts and Builds binaries
export PATH="$PATH:$HOME/Scripts:$HOME/Builds/bin:$HOME/.bin/"

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"


# General bashrc -----------------------------------------------

# Set window title to PWD
case $TERM in
  xterm*)
  PROMPT_COMMAND='echo -ne "\033]0; $PWD \007";'
  ;;
esac

# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac

# History and Shopt ------------

# Don't put duplicate lines or lines starting with space in the history.
HISTCONTROL=ignoreboth
HISTSIZE=1000
HISTFILESIZE=2000

# Append to the history file; don't overwrite.
shopt -s histappend

# Check the window size after each command and update LINES and COLUMNS.
shopt -s checkwinsize

# Used in a path expansion, match all files, directories, subdirectories.
shopt -s globstar

# Bash Completions -------------

# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).

if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi


# Command Prompt -----------------------------------------------

PROMPT_DIRTRIM=2

GIT_PS1_SHOWDIRTYSTATE=1
GIT_PS1_SHOWUNTRACKEDFILES=1
GIT_PS1_SHOWCOLORHINTS=1
GIT_PS1_SHOWUPSTREAM=auto

set_prompt () {
    PS1=''

    # color escape codes
    local off='\[\033[0m\]'
    local blue='\[\033[1;34m\]'
    local purple='\[\033[0;35m\]'
    local gray='\[\033[0;37m\]'

    PS1+=$blue'\D{%T} '
    PS1+=$purple'\u'$off':'$purple'\W '
    PS1+='\[\033[1;92m\]$(__git_ps1 "(%s)") ›\[\033[0m\] '
}

case "$TERM" in
    xterm*|rxvt*)
        PROMPT_COMMAND='set_prompt' ;;

    *color*)
        PROMPT_COMMAND='set_prompt' ;;

    *)
        PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ ' ;;
esac


# Misc ---------------------------------------------------------

# Dir Colors -------------------
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"

    alias ls='ls --color=auto'
    alias grep='grep --color=auto'
fi

# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# NVM --------------------------

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
