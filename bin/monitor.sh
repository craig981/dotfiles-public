#!/bin/bash

if [[ "$XDG_CURRENT_DESKTOP" = "MATE" ]]; then
    gsettings set org.mate.power-manager idle-dim-battery false
fi

# switch off laptop screen when external monitor connected
[[ $(xrandr | awk '/^HDMI-1/{print $2}') == "connected" ]] && xrandr --output eDP-1 --off
