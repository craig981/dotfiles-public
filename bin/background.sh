#!/bin/bash


if [[ "$XDG_CURRENT_DESKTOP" = "MATE" ]]; then
    [ -f ~/Pictures/background ] && gsettings set org.mate.background picture-filename ~/Pictures/background
    [ -d ~/Pictures/background ] && gsettings set org.mate.background picture-filename \
	"${HOME}/Pictures/background/$(/bin/ls -1 ${HOME}/Pictures/background | sort -R | head -n 1)"
    gsettings set org.mate.background show-desktop-icons false
    gsettings set org.mate.background picture-options zoom # or centered
fi

if [[ "$XDG_CURRENT_DESKTOP" = "i3" ]]; then
    [ -f ~/Pictures/background ] && feh --no-fehbg --bg-fill ~/Pictures/background
    [ -d ~/Pictures/background ] && feh --no-fehbg --bg-fill -z ~/Pictures/background
fi

