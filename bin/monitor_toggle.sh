#!/bin/bash

# toggle laptop screen
if $(xrandr | awk '/^eDP-1/{p=1; print; next} /^[^ ]/{p=0} p==1{print}' | grep -q -F '*'); then
    mode=--off
else
    mode=--auto
fi
set -x
xrandr --output eDP-1 $mode
