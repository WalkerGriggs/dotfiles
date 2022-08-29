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

# Keyboard Layout -------------
alias ANSI='setxkbmap us'
alias JIS='setxkbmap jp'
alias XMM_STD='setxkbmap us && xmodmap ~/.Xmodmap_std'
alias XMM_WKL='setxkbmap us && xmodmap ~/.Xmodmap_wkl'

export FONTCONFIG_PATH=/etc/fonts

# Golang ----------------------
export GOPATH=$HOME/go

# Git -------------------------
export GPG_TTY=$(tty)
export GIT_TERMINAL_PROMPT=1

# General Utils ---------------
alias reload='xrdb $HOME/.Xresources & source $HOME/.bashrc & i3-msg restart'
alias wifi='nmtui'
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

export PATH="$PATH:$HOME/.local/bin"

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

function _update_ps1() {
    GIT_PS1_SHOWDIRTYSTATE=1
    PS1='\w$(__git_ps1 " (%s)") ∴ '
}

case "$TERM" in
    xterm*|rxvt*)
        PROMPT_COMMAND='_update_ps1' ;;

    *color*)
        PROMPT_COMMAND='_update_ps1' ;;

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
