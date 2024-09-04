#!/bin/bash

sleep 2

~/dotfiles-public/bin/monitor.sh
~/dotfiles-public/bin/keyboard.sh
~/dotfiles-public/bin/trackpad.sh
~/dotfiles-public/bin/background.sh

if [[ "$XDG_CURRENT_DESKTOP" = "ubuntu:GNOME" ]]; then
    gsettings set org.gnome.desktop.privacy remember-recent-files false
    gsettings set org.gtk.Settings.FileChooser show-hidden false
fi
