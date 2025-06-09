
# sourced for non-interactive shells (e.g. emacs compilation mode)

if [[ "$(uname -s)" = "Darwin" ]]; then
  # macports
  export PATH="/opt/local/bin:/opt/local/sbin:$PATH"

  # local builds from source ahead of macports
  export PATH="/usr/local/bin:${PATH}"
fi

export PATH="$HOME/dotfiles-public/bin:$HOME/.local/bin:${PATH}"
