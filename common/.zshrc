# -*- mode: sh; sh-shell: zsh; -*-

# emacs keys
bindkey -e
zle -A emacs-forward-word forward-word

# make alt+backspace/f/b/d work with directories
WORDCHARS=${WORDCHARS//[\/.-]}

HISTSIZE=10000
SAVEHIST=10000
setopt INC_APPEND_HISTORY
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_SAVE_NO_DUPS

setopt NO_BEEP NO_LIST_BEEP LIST_AMBIGUOUS
setopt NO_REC_EXACT AUTO_LIST LIST_PACKED
#setopt MENU_COMPLETE
#setopt AUTO_MENU

# highlight selection in menu
zmodload zsh/complist
autoload -Uz compinit
compinit
# first tab shows completions, second tab highlights
zstyle ':completion:*' menu select
# complete hidden files
_comp_options+=(globdots)

# hit return once instead of twice
bindkey -M menuselect '^M' .accept-line

# shift+tab back
bindkey -M menuselect '^[[Z' reverse-menu-complete

bindkey -M menuselect '^D' accept-line
setopt noflowcontrol
bindkey -M menuselect '^S' forward-char
bindkey -M menuselect '^R' backward-char

# ctrl-a/e exits menu
bindkey -M menuselect '^A' .beginning-of-line
bindkey -M menuselect '^E' .end-of-line

# don't error if glob fails to match
unsetopt nomatch


test -f ~/dotfiles-public/bash_zsh_alias && source ~/dotfiles-public/bash_zsh_alias
test -f ~/dotfiles/bash_zsh_alias && source ~/dotfiles/bash_zsh_alias

if [[ "$(uname -s)" = "Linux" ]]; then
    test -f ~/dotfiles-public/common/.zshenv && source ~/dotfiles-public/common/.zshenv
fi

autoload -U colors && colors

PS1="%{$fg[cyan]%}%n@%m %2~ %#%{$reset_color%} "

