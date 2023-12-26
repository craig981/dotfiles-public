#!/bin/bash

if [[ $# != 1 ]]; then
    echo "usage: brightness.sh <delta>" 1>&2
    exit 1
fi

device=acpi_video0
brightness=/sys/class/backlight/${device}/brightness

if [[ ! -w "${brightness}" ]]; then
    echo "brightness.sh : cannot write to ${brightness}, check if group is video" 1>&2
    exit 1
fi

min=1
max=$(cat "/sys/class/backlight/${device}/max_brightness")
val=$(cat "${brightness}")

val=$(( val + $1 ))
val=$(( val > max ? max : val ))
val=$(( val < min ? min : val ))

echo "$val" > "${brightness}"
