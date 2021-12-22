
osuname := $(shell uname -s)

all:
	[ -d $(HOME)/dotfiles ] && $(MAKE) -C $(HOME)/dotfiles all

	stow -v common

ifeq ($(osuname),Linux)
	stow -v gtk
ifeq (${XDG_CURRENT_DESKTOP},i3)
	stow -v i3
endif
endif
ifeq ($(osuname),Darwin)
	stow -v cmus
endif

	[ -f ~/.vim/autoload/plug.vim ] || \
		curl -o ~/.vim/autoload/plug.vim --create-dirs \
		https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

	[ -d ~/.terminfo ] && find ~/.terminfo -name dumb-emacs-ansi | grep -q . || tic dumb-emacs-ansi.ti


macports-install:
	sudo port install \
		tree htop the_silver_searcher git \
		vim neovim tmux universal-ctags cmake stow \
		cmus ImageMagick ffmpeg \
		aspell aspell-dict-en

macports-selfupdate:
	sudo port selfupdate
	port outdated

macports-upgrade:
	sudo port upgrade outdated

macports-remove-old:
	sudo port uninstall inactive


apt-install:
	sudo apt-get install \
		tree htop silversearcher-ag git \
		neovim universal-ctags cmake stow \
		cmus imagemagick pasystray feh

security-updates-dry-run:
	sudo apt-get update
	sudo unattended-upgrade -d --dry-run
security-updates:
	sudo apt-get update
	sudo unattended-upgrade -d

update-upgrade:
	# update list of available packages, does not install or upgrade packages
	sudo apt-get update
	# install newer version of packages
	sudo apt-get upgrade

update-gui:
	update-manager

update-chrome:
	sudo apt-get install --only-upgrade google-chrome-stable

