#!/bin/bash

case "$(hostname -s)" in
    goose|hedgehog) exit 0 ;;
esac

echo \
LA  `env TZ=America/Los_Angeles date +%R` " " \
NY  `env TZ=America/New_York date +%R` " " \
LON `env TZ=Europe/London date +%R` " " \
STH `env TZ=Europe/Stockholm date +%R`

