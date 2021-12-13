#!/bin/tcsh -f

who -u | awk '{print $1}' | grep -v $USER | sort -u | tr '\n' ' '

