#!/bin/bash

id=$(xinput list | awk '/Touchpad/{print $(NF-3)}' | cut -d= -f2)
if [[ -n "${id}" ]]; then
    # disable touchpad when mouse plugged in
    if [[ $(xinput list | grep -i mouse) ]]; then
	xinput disable "${id}"
    else
	xinput enable "${id}"
    fi
fi

