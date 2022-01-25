#!/bin/bash

# back to default: setxkbmap -option
# console: man keyboard;  /etc/default/keyboard

setxkbmap -option
setxkbmap -option caps:ctrl_modifier
setxkbmap -option ctrl:ralt_rctrl
setxkbmap -option altwin:menu_win

if [[ "$XDG_CURRENT_DESKTOP" = "MATE" ]]; then
    gsettings set org.mate.interface gtk-key-theme Emacs
fi

# # mac keyboard
# setxkbmap -option
# setxkbmap -option caps:ctrl_modifier
# setxkbmap -option ctrl:swap_rwin_rctl
# setxkbmap -option altwin:swap_lalt_lwin
