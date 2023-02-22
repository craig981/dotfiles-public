
osuname := $(shell uname -s)

all:
	[ -d "${HOME}/.emacs.d" ] || mkdir -p "${HOME}/.emacs.d"
	[ -d $(HOME)/dotfiles ] && $(MAKE) -C $(HOME)/dotfiles all

	stow -v common

ifeq ($(osuname),Linux)
	stow -v gtk
# ifeq (${XDG_CURRENT_DESKTOP},mate)
	# stow -v mate
# endif
endif
ifeq ($(osuname),Darwin)
	stow -v cmus
	mkdir -p ~/Library/KeyBindings
	cp -v mac/Library/KeyBindings/DefaultKeyBinding.dict ~/Library/KeyBindings/
endif

	[ -f ~/.vim/autoload/plug.vim ] || \
		curl -o ~/.vim/autoload/plug.vim --create-dirs \
		https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

	[ -d ~/.terminfo ] && find ~/.terminfo -name dumb-emacs-ansi | grep -q . || tic dumb-emacs-ansi.ti


macports-install:
	sudo port install \
		ImageMagick \
		aspell \
		aspell-dict-en \
		cmake \
		cmus \
		ffmpeg \
		git \
		gnuplot \
		htop \
		ledger \
		pandoc \
		stow \
		the_silver_searcher \
		tmux \
		tree \
		universal-ctags

macports-selfupdate:
	sudo port selfupdate
	port outdated

macports-upgrade:
	sudo port upgrade outdated

macports-remove-old:
	sudo port uninstall inactive


apt-install:
	sudo apt-get install \
		cmake \
		cmus \
		feh \
		git \
		htop \
		imagemagick \
		silversearcher-ag \
		stow \
		tree \
		universal-ctags

# /etc/apt/apt.conf.d/50unattended-upgrades
security-updates-dry-run:
	sudo apt-get update
	sudo unattended-upgrade -v --dry-run
security-updates:
	sudo apt-get update
	sudo unattended-upgrade -v

update-upgrade:
	# update list of available packages, does not install or upgrade packages
	sudo apt-get update
	# install newer version of packages
	sudo apt-get upgrade

update-gui:
	update-manager

update-chrome:
	sudo apt-get install --only-upgrade google-chrome-stable

