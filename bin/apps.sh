#!/bin/bash

if [[ "$XDG_CURRENT_DESKTOP" = "ubuntu:GNOME" ]]; then
    gsettings set org.gnome.desktop.privacy remember-recent-files false
    gsettings set org.gtk.Settings.FileChooser show-hidden false


    # no terminal bell noise
    profile=$(gsettings get org.gnome.Terminal.ProfilesList default | tr -d "'")
    if [[ -n "${profile}" ]]; then
        key="/org/gnome/terminal/legacy/profiles:/:${profile}"
        dconf write "${key}/audible-bell" false
        dconf write "${key}/cursor-blink-mode" "'off'"
    fi


    # # make gnome-terminal open tmux
    # if command -v tmux > /dev/null 2>&1; then
    #     key=$(~/dotfiles-public/bin/term_profile.sh)
    #     if [[ $? -eq 0 && -n "${key}" ]]; then
    #         dconf write "${key}/use-custom-command" true
    #         dconf write "${key}/custom-command" "'tmux'"
    #     fi
    # fi
fi

if [[ "$XDG_CURRENT_DESKTOP" = "MATE" ]]; then

    dconf write /org/mate/terminal/profiles/default/default-size-columns 150
    dconf write /org/mate/terminal/profiles/default/default-size-rows 40

fi
