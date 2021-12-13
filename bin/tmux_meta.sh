#!/bin/bash

# make key $1 (e.g. "M-o") switch panes with the remaining args
# (e.g. "-t :.+"), or pass the key through if there's only one pane

num_panes=$(tmux list-panes -F "#{pane_id}" | wc -l)
#name=$(tmux list-windows -F "#{window_active} #{window_name}" | awk '$1=="1"{print $2}')
if (( "${num_panes}" == 1 )); then
	tmux send-keys "$1"
else
	shift
	tmux select-pane "$@"
fi
