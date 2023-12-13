#!/bin/bash

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
