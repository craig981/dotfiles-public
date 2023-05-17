# -*- mode: sh; sh-shell: tcsh; -*-

# sourced by ~/dotfiles/myrc rather than symlinked


alias d		'imf_disp'
alias ls	'/bin/ls --color=auto'
alias ll	'/bin/ls -lrth --color=auto'
alias la	'/bin/ls -lrthA --color=auto'
alias e		'emacs -nw'
alias tmux	'tmux -2'

alias abcll	'abcls -almrstv'
alias km	'killall -s 9 maya.bin'
alias kf	'killall -s 9 firefox-bin'
alias ke	'killall -s 9 emacs'

setenv EDITOR vim
setenv SVN_EDITOR vim

bindkey "^U" backward-kill-line
bindkey "^R" i-search-back
bindkey "^W" kill-region
# C-space
bindkey '^\040' set-mark-command
# space expands !* !^ !:n !:n-m !:n-$ etc
bindkey ' ' magic-space

complete git 'p@1@`~/dotfiles-public/bin/list_git_cmds.csh`@@'

alias g 'git st'
# alias gd 'git di'
# alias gdc 'git di --cached'
# alias ga 'git add -p'
# alias gc 'git commit'
alias gl 'git lo'

setenv LS_COLORS 'no=00:fi=00:di=38;5;69:ow=38;5;69:ln=01;33:pi=04;33:*.dylib=04;03;31:so=01;35:bd=03;32;03:cd=03;36;02:or=30;01;31:ex=01;32'

if ( $?prompt ) then
  # if ( "$TERM" == "dumb" ) then
    # set prompt='$ '
  # else
    set PROMPT_COLOR='38;5;242'
    set prompt='%B%m%B %{^[[%$PROMPT_COLOR;1m%}%c2%{^[[0m%} > '
  # endif

    if ( $?INSIDE_EMACS ) then
	if ( "$TERM" == "dumb" ) then
		setenv TERM dumb-emacs-ansi
		setenv COLORTERM 1
	endif
	setenv LS_COLORS 'no=00:fi=00:di=1;34:ow=1;34:ln=01;33:pi=04;33:*.dylib=04;03;31:so=01;35:bd=03;32;03:cd=03;36;02:or=30;01;31:ex=01;32'
    endif
endif

# make ctrl-D exit
set ignoreeof=1

set history=10000

# treat . and _ as punctuation for word delete
set wordchars=
