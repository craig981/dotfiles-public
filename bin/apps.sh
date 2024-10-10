#!/bin/bash

if [[ "$XDG_CURRENT_DESKTOP" = "ubuntu:GNOME" ]]; then
    gsettings set org.gnome.desktop.privacy remember-recent-files false
    gsettings set org.gtk.Settings.FileChooser show-hidden false
fi
