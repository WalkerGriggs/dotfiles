# ------------------------------------------------------------
# file:        xcontrol.sh
# author:      WalkerGriggs     www.walkergriggs.com
# date:        01_16_17
# ------------------------------------------------------------

### Y88b   d88P    .d8888b.                 888                  888
###  Y88b d88P    d88P  Y88b                888                  888
###   Y88o88P     888    888                888                  888
###    Y888P      888        .d88b. 88888b. 888888888d888 .d88b. 888
###    d888b      888       d88""88b888 "88b888   888P"  d88""88b888
###   d88888b     888    888888  888888  888888   888    888  888888
###  d88P Y88b    Y88b  d88PY88..88P888  888Y88b. 888    Y88..88P888
### d88P   Y88b    "Y8888P"  "Y88P" 888  888 "Y888888     "Y88P" 888
###                                             An X11 Theme Manager

CURPATH=$(dirname "$0")
CONFPATH=~/.colors/
XPATH=~/.Xresources

function welcome {
    # Prints figlet ascii art starting with '###'
    clear
    cat "$0" | grep -e "^###" | sed "s/^###//g"
    echo ""
}

function count_dir {
    # Returns the number of files in a given directory
    a=$(($(ls -l $1 | wc -l)-1))
    return $a
}

function print_list {
    # Prints bash array and enumerates indexing from 0
    local INDEX=0
    while [ $# -gt 0 ]
    do
        INDEX=$(($INDEX+1))
        printf " %2d. %s\n" $INDEX "$1"
        shift
    done
}

function select_from_list {
    # Runs actual selection. Reads CLI input and returns if valid.
    count_dir $@
    local COUNT=$?

    cd $1
    files=(*)
    print_list ${files[@]}

    echo ""
    while true
    do
        printf "Please enter a number: "

        read INPUT </dev/tty
        [ "$INPUT" == "q" ] && exit
        INPUT=$(($INPUT))
        if [ $INPUT -ge 1 -a $INPUT -le $COUNT ]
        then
            return $INPUT
        fi
    done
    return $INPUT
}

function copy_config {
    # Copies configuration content into a THEME_INCLUDE
    # THEME_INCLUDE should be included in .Xresources
    echo "Using configuration file:" $1
    if [ -f $CONFPATH/$1 ] ; then
        cp $CONFPATH/$1 $CONFPATH/THEME_INCLUDE
        xrdb $XPATH # reload .Xresources and .bashrc with alias
    else
        echo "'$path$1' is not a valid file."
        echo "Usage: set_theme <theme found in ~/.colors>"
    fi
    echo "Configuration complete"
}

# Yo DJ spin that track
welcome
select_from_list $CONFPATH
copy_config ${files[$(($?))-1]}
