#!/bin/bash

img=$(readlink -m ~/Pictures/background)

if [[ "$XDG_CURRENT_DESKTOP" = "MATE" ]]; then
    [ -f "${img}" ] && gsettings set org.mate.background picture-filename "${img}"
    [ -d "${img}" ] && gsettings set org.mate.background picture-filename \
	"${HOME}/Pictures/background/$(/bin/ls -1 ${HOME}/Pictures/background | sort -R | head -n 1)"
    gsettings set org.mate.background show-desktop-icons false
    gsettings set org.mate.background picture-options zoom # or centered
fi

if [[ "$XDG_CURRENT_DESKTOP" = "ubuntu:GNOME" ]]; then
    gsettings set org.gnome.desktop.background picture-options 'zoom'
    # gsettings set org.gnome.desktop.background picture-options 'scaled'
fi

if [[ "$XDG_CURRENT_DESKTOP" = "i3" ]]; then
    [ -f ~/Pictures/background ] && feh --no-fehbg --bg-fill ~/Pictures/background
    [ -d ~/Pictures/background ] && feh --no-fehbg --bg-fill -z ~/Pictures/background
fi

