#!/bin/tcsh -f

find `git --exec-path` -maxdepth 1 \( -type f -o -type l \) -name 'git-*' | xargs -n 1 basename | grep -v -e '--' | sed 's/^git-\(.*\)/\1/' | sort -u
