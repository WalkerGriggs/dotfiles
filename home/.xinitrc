#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap_std
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

DEFAULT_SESSION=i3
case $1 in
    i3)
        $HOME/.screenlayout/school.sh
        xmodmap ~/.Xmodmap_std
        gnome-settings-daemon

        exec i3
        ;;
    plasma)
        exec startkde
        ;;
    *)
        exec i3
        ;;
esac
