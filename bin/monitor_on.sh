#!/bin/bash

# find virtual terminal for X (e.g. tty2), switch laptop screen back on
tty=$(ps -e | awk '/Xorg/{print $2}' | tr -d '[a-z]')
if [[ -z "${tty}" ]]; then
    echo "failed to find tty for Xorg"
    exit 1
fi
echo "Xorg on tty${tty}"
chvt ${tty}
sleep 2
env DISPLAY=:0 xrandr --output eDP-1 --auto
