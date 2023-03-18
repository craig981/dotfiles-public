#!/bin/bash

if [[ $(uname) == "Darwin" ]]; then
    sleep $(( 5*60 ))
    osascript -e "display notification \"$(date +%H:%M:%S)\" with title \"Coffee\""
    exit 0
fi

future=$((`date +%s` + 5*60));
while [[ "$future" -ge `date +%s` ]]; do
    clear
    echo -ne "$(date -u --date @$(( $future - `date +%s` )) +%M:%S)" | figlet -f roman
    sleep 1
done
date
