
osuname := $(shell uname -s | cut -d- -f1)

all:
	[ -d "${HOME}/.emacs.d" ] || mkdir -p "${HOME}/.emacs.d"

	stow -v common
	stow -v vim

ifeq ($(osuname),Linux)
	stow -v emacs
endif
ifeq ($(osuname),Darwin)
	stow -v emacs
	stow -v emacs_mac
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

ifeq ($(osuname),Linux)
startup:
	stow -v gnome
endif

macports-install:
	sudo port -N install \
		ImageMagick \
		aspell \
		aspell-dict-en \
		bash \
		cmake \
		feh \
		ffmpeg \
		gawk \
		gnuplot \
		htop \
		jq \
		mpv \
		pandoc \
		pinentry-mac \
		ripgrep \
		rsync \
		stow \
		texlive-latex \
		texlive-latex-recommended \
		texlive-fonts-recommended \
		tmux \
		tree \
		universal-ctags \
		wget

mac-gpg-init:
	echo 'pinentry-program /Applications/MacPorts/pinentry-mac.app/Contents/MacOS/pinentry-mac' >> ~/.gnupg/gpg-agent.conf
	gpgconf --kill gpg-agent

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

apt-install-debug:
	sudo apt-get install gdb gdb-doc

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

# "Could not figure out development release: Distribution data outdated. Please check for an update for distro-info-data"
update-distro-info-data:
	sudo apt install --only-upgrade distro-info-data

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
