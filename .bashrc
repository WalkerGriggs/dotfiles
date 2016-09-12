# ---------------------------------------------------------------------
# file:		~/.bashrc
# author:  	Walker Griggs	- www.walkergriggs.com
# date:		06/18/2016    
# ---------------------------------------------------------------------

set -o noclobber # don't overwrite files

# Aliases -------------------------------------------------------------

alias reload='xrdb ~/.Xresources & source ~/.bashrc'
alias wifi='nmtui'
alias news='newsbeuter -r'
alias pingg='ping -c 3 www.google.com'

alias ls='ls -h --color'
alias la='ls -a'
alias ll='ls -l'
alias lla='ls -al'
alias date='date "+%F %T"'
alias ..='cd ..'

alias reboot="sudo shutdown -r now"
alias shutdown="sudo shutdown -h now"

alias install='sudo apt-get install'
alias remove='sudo apt-get remove'
alias update='sudo apt-get update'
alias upgrade='sudo apt-get update && sudo apt-get upgrade'

# WatchDogs Term ------------------------------------------------------
watchDogs=$(</home/wpgriggs/.watchDogs/.watchVar)
if [ $watchDogs == true ]; then
	sleep .5
	IFS='%'
	CYAN='\033[1;36m'
	GREEN='\033[1;32m'
	WHITE='\033[1;97m'
	while read line; do
		printf "${GREEN}$line\n"
		sleep .04
	done</home/wpgriggs/.watchDogs/.dedText
fi

# General bashrc ------------------------------------------------------

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
#[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    if [ $watchDogs == false ]; then
        #PS1="┌─[\[\e[0;32m\]\u\e[m@\e[0;35m\h \[\e[1;31m\]\w\[\e[0m\]]\n└─→ " #decorated
    	PS1="\[\e[1;34m\]\u@\[\e[1;37m\]\h \[\e[1;30m\]\w $\[\e[m\] " #simple
    else
        # PS1="\[\e[30m\]\e[106m _ctOS_ apearce \[\e[0m \e[1;30m\]\w \[\e[97m\]" # ctOS
        # PS1="\[\e[97m\]Blume_ctOS t-bone@slave_A591185 \e[1;30m\]\w $ \[\e[97m\]" # Blume
    	PS1="\[\e[1;32m\]C:/DEDSEC_TAKEOVER mholloway \w ////\[\e[0m \[\e[1;30m\]\[\e[0m\]" # Dedsec
    fi
    # Default prompt: PS1='\[\e]0\e[0;31m;\u\e[m@\h: \w\a\]${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    #alias grep='grep --color=auto'
    #alias fgrep='fgrep --color=auto'
    #alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi


# Functions -----------------------------------------------------------
welcome() {
    figlet "Hey, " $USER "!";
    echo -e ""; neofetch
    echo -e ""; cal
    echo -ne "Up time:";uptime | awk /'up/'
    echo -en "Local IP Address :"; /sbin/ifconfig wlan0 | awk /'inet addr/ {print $2}' | sed -e s/addr:/' '/ 
    echo -e ""; df -h /dev/dm-1
    echo -e ""; fortune
    echo "";
}

extract () {
  if [ -f $1 ] ; then
      case $1 in
          *.tar.bz2)   tar xvjf $1    ;;
          *.tar.gz)    tar xvzf $1    ;;
	  *.tar.xz)    tar xvjf $1    ;;
          *.bz2)       bunzip2 $1     ;;
          *.rar)       rar x $1       ;;
          *.gz)        gunzip $1      ;;
          *.tar)       tar xvf $1     ;;
          *.tbz2)      tar xvjf $1    ;;
          *.tgz)       tar xvzf $1    ;;
          *.zip)       unzip $1       ;;
          *.Z)         uncompress $1  ;;
          *.7z)        7z x $1        ;;
	  *.xz)        unxz $1	      ;;
          *)           echo "don't know how to extract '$1'..." ;;
      esac
  else
      echo "'$1' is not a valid file!"
  fi
}

dotfiles () {
  foo=/home/wpgriggs/Documents/DotFiles
  while IFS=, read xx yy; do
    echo $foo/$yy
    mkdir -p $foo/$yy 
    cp -rvu $xx $foo/$yy
  done < /home/wpgriggs/Documents/DotFiles/include.csv
}

watchDogs() {
	if [ $watchDogs == false ]; then
		rm /home/wpgriggs/.watchDogs/.watchVar
		echo true > /home/wpgriggs/.watchDogs/.watchVar
	else
		rm /home/wpgriggs/.watchDogs/.watchVar
		echo false > /home/wpgriggs/.watchDogs/.watchVar
	fi
	reload
}
