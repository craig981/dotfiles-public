#!/bin/bash

if [[ "$XDG_CURRENT_DESKTOP" = "MATE" ]]; then
    gsettings set org.mate.power-manager idle-dim-battery false
fi

# if [[ "$(hostname)" = "goose" ]]; then
#     brightnessctl -d acpi_video0 s 8
# fi

# Chrome fuzzy/soft font rendering.
# Open "chrome://flags", search "ozone-platform-hint", set to Auto.

# switch off laptop screen when external monitor connected
#[[ $(xrandr | awk '/^HDMI-1/{print $2}') == "connected" ]] && xrandr --output eDP-1 --off
