#!/bin/tcsh -f

echo LA `env TZ=America/Los_Angeles date +%R` " " \
CHI `env TZ=America/Chicago date +%R` " " \
NY `env TZ=America/New_York date +%R` " " \
BER `env TZ=Europe/Berlin date +%R` " " \
BLR `env TZ=Asia/Kolkata date +%R`

