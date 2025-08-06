# -*- mode: sh; sh-shell: bash; -*-
# ~/.bashrc: executed by bash(1) for non-login shells

export PATH=$HOME/tools/bin:$HOME/dotfiles-public/bin:$HOME/.local/bin:/usr/local/bin:${PATH}

export LESSHISTFILE=/dev/null

unset DEBUGINFOD_URLS


# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history
HISTCONTROL=ignoreboth
HISTFILESIZE=100000
HISTSIZE=
HISTTIMEFORMAT="%Y/%m/%d %a %T "

# append to the history file, don't overwrite
shopt -s histappend

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

case "$TERM" in
    emacs) ;;
    dumb) ;;
    *)
    bind -u complete-filename
    bind '"\e/": dabbrev-expand'
    ;;
esac

test -f ~/dotfiles-public/bash_zsh_alias && source ~/dotfiles-public/bash_zsh_alias
test -f ~/dotfiles/bash_zsh_alias && source ~/dotfiles/bash_zsh_alias

case "$(uname -s)" in
    Darwin)
        PS1='\[\033[0;36m\]\u@\h \W \$\[\033[00m\] '
        ;;
    *)
    PS1='\[\033[0;36m\][\D{%H:%M}] \u@\h \W \$\[\033[00m\] '
    ;;
esac

