#!/bin/bash

if [[ "$XDG_CURRENT_DESKTOP" = "ubuntu:GNOME" ]]; then
    gsettings set org.gnome.desktop.peripherals.touchpad tap-to-click false
    gsettings set org.gnome.desktop.peripherals.touchpad natural-scroll false
    gsettings set org.gnome.desktop.wm.preferences resize-with-right-button true
fi

if [[ "$XDG_CURRENT_DESKTOP" = "MATE" ]]; then
    gsettings set org.mate.peripherals-touchpad tap-to-click false
fi

if [[ "$(hostname)" = "asusbox" ]]; then
    id=$(xinput list | awk '/Touchpad/{print $(NF-3)}' | cut -d= -f2)
    if [[ -n "${id}" ]]; then
	# disable touchpad when mouse plugged in
	if [[ $(xinput list | grep -i mouse) ]]; then
	    xinput disable "${id}"
	else
	    xinput enable "${id}"
	fi
    fi
fi

