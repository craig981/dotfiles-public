#!/bin/bash

if [[ "$XDG_CURRENT_DESKTOP" = "ubuntu:GNOME" ]]; then
    # automatic screen brightness off
    gsettings set org.gnome.settings-daemon.plugins.power ambient-enabled false
    # show battery percentage
    gsettings set org.gnome.desktop.interface show-battery-percentage true
    # no home icon on desktop
    gsettings set org.gnome.shell.extensions.ding show-home false
    # dock panel mode off
    gsettings set org.gnome.shell.extensions.dash-to-dock extend-height false
    # screen blank
    gsettings set org.gnome.desktop.session idle-delay 1800
fi

if [[ "$XDG_CURRENT_DESKTOP" = "MATE" ]]; then
    gsettings set org.mate.power-manager idle-dim-battery false
fi

# if [[ "$(hostname)" = "goose" ]]; then
#     brightnessctl -d acpi_video0 s 8
# fi

# Chrome
#
# Fix fuzzy/soft font rendering.
# chrome://flags/#ozone-platform-hint Auto
#
# Remove drop-down menu at the left of the tab bar.
# chrome://flags/#customize-chrome-side-panel Disable

# switch off laptop screen when external monitor connected
#[[ $(xrandr | awk '/^HDMI-1/{print $2}') == "connected" ]] && xrandr --output eDP-1 --off
