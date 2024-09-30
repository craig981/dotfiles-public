
osuname := $(shell uname -s | cut -d- -f1)

all:
	[ -d "${HOME}/.emacs.d" ] || mkdir -p "${HOME}/.emacs.d"
	#[ -d $(HOME)/dotfiles ] && $(MAKE) -C $(HOME)/dotfiles all

	stow -v common

ifeq ($(osuname),Linux)
	stow -v emacs
	stow -v gnome
endif
ifeq ($(osuname),Darwin)
	stow -v emacs
	mkdir -p ~/Library/KeyBindings
	cp -v mac/Library/KeyBindings/DefaultKeyBinding.dict ~/Library/KeyBindings/

	# tic -o ~/.terminfo /Applications/Emacs.app/Contents/Resources/etc/e/eterm-color.ti
endif
ifeq ($(osuname),CYGWIN_NT)
	cp -v win/emacs.bat win/.emacs ~/
	cp -v emacs/.emacs.d/my-override-*-theme.el ~/.emacs.d/
	unlink ~/.gitconfig && cp ~/dotfiles-public/common/.gitconfig ~/

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
		ffmpeg \
		git \
		gnuplot \
		htop \
		jq \
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
		bash-doc \
		cmake \
		curl \
		feh \
		ffmpeg \
		gawk \
		git \
		gnuplot \
		gnutls-bin \
		htop \
		imagemagick \
		jq \
		ledger \
		mplayer \
		mpv \
		pandoc \
		patchelf \
		ripgrep \
		stow \
		texlive-latex-base \
		texlive-latex-recommended \
		tcsh \
		tree \
		tmux \
		universal-ctags \
		vlc \
		xsel

apt-install-prof:
	sudo apt-get install			\
		valgrind			\
		kcachegrind			\
		heaptrack			\
		heaptrack-gui

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

ssp:
	sudo btmgmt ssp on
