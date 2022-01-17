
# sourced for non-interactive shells (e.g. emacs compilation mode)

if [[ "$(uname -s)" = "Darwin" ]]; then
  # macports
  export PATH="/opt/local/bin:/opt/local/sbin:$PATH"

  export PATH="$PATH:/usr/local/texlive/2015basic/bin/x86_64-darwin"
fi

export PATH="$HOME/tools/bin:$HOME/dotfiles-public/bin:${PATH}"
export PATH="$HOME/.cargo/bin:$PATH"
