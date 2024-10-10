#!/bin/bash

profile=$(dconf dump /org/gnome/terminal/ | \
              grep -m 1 '^\[legacy/profiles' | \
              tr -d '[]')

if [[ -z "${profile}" ]]; then
    exit 1
fi

echo "/org/gnome/terminal/${profile}"
