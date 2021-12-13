#!/bin/bash

# switch off laptop screen when external monitor connected
[[ $(xrandr | awk '/^HDMI-1/{print $2}') == "connected" ]] && xrandr --output eDP-1 --off
