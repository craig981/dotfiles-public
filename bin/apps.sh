#!/bin/bash

if [[ "$XDG_CURRENT_DESKTOP" = "ubuntu:GNOME" ]]; then
    gsettings set org.gnome.desktop.privacy remember-recent-files false
    gsettings set org.gtk.Settings.FileChooser show-hidden false

    # make gnome-terminal open tmux
    if command -v tmux > /dev/null 2>&1; then
        key=$(~/dotfiles-public/bin/term_profile.sh)
        if [[ $? -eq 0 && -n "${key}" ]]; then
            dconf write "${key}/use-custom-command" true
            dconf write "${key}/custom-command" "'tmux'"
        fi
    fi
fi
