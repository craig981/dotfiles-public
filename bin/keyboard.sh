#!/bin/bash

# back to default: setxkbmap -option
# console: man keyboard;  /etc/default/keyboard

setxkbmap -option
setxkbmap -option caps:ctrl_modifier
setxkbmap -option ctrl:ralt_rctrl
setxkbmap -option altwin:menu_win
# setxkbmap -option shift:both_capslock

for id in $(xinput list | sed -e '1,/Virtual core keyboard/d' | grep -e 'Keychron' -e 'PCoIP.*Keyboard' -e 'RGS keyboard' | sed -e 's/.*id=\([0-9]\+\).*/\1/')
do
	setxkbmap -device "${id}" -option
	setxkbmap -device "${id}" -option caps:ctrl_modifier
	setxkbmap -device "${id}" -option ctrl:swap_rwin_rctl
	setxkbmap -device "${id}" -option altwin:swap_lalt_lwin
	# setxkbmap -device "${id}" -option shift:both_capslock
done

if [[ "$XDG_CURRENT_DESKTOP" = "MATE" ]]; then
    gsettings set org.mate.interface gtk-key-theme Emacs
    # disable Super_L key opening menu
    gsettings set org.mate.mate-menu hot-key ''
    gsettings set com.solus-project.brisk-menu hot-key ''
    # disable Super_L-E opening file explorer
    gsettings set org.mate.Marco.global-keybindings run-command-3 ''
fi

# https://docs.microsoft.com/en-us/sysinternals/downloads/ctrl2cap
