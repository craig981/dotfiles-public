#!/bin/bash

if [[ "$XDG_CURRENT_DESKTOP" = "ubuntu:GNOME" ]]; then

    # previously in /etc/default/keyboard; man keyboard; sudo dpkg-reconfigure keyboard-configuration
    # On hedgehog
    # XKBOPTIONS="caps:ctrl_modifier,ctrl:ralt_rctrl,ctrl:rctrl_ralt,shift:both_capslock"
    # On goose
    # XKBOPTIONS="caps:ctrl_modifier,altwin:swap_lalt_lwin,ctrl:swap_rwin_rctl,shift:both_capslock"

    case "$(hostname)" in
	"goose")
	    # system
	    gsettings set org.gnome.shell.keybindings toggle-overview "['LaunchA']"
	    # stop Super_L tap showing window overview
	    gsettings set org.gnome.mutter overlay-key ''

	    gsettings set org.gnome.desktop.input-sources xkb-options "['caps:ctrl_modifier', 'altwin:swap_lalt_lwin', 'ctrl:swap_rwin_rctl', 'shift:both_capslock', 'lv3:ralt_alt']"
	    # gsettings set org.gnome.desktop.input-sources xkb-options "['caps:escape_shifted_capslock', 'ctrl:swap_lalt_lctl_lwin', 'ctrl:swap_rwin_rctl']"
	    ;;

	"hedgehog")
	    if [[ -z $(xinput list --name-only | grep -i 'Apple.*Magic Keyboard') ]]; then
		gsettings set org.gnome.desktop.input-sources xkb-options "['caps:ctrl_modifier', 'ctrl:ralt_rctrl', 'ctrl:rctrl_ralt', 'shift:both_capslock']"
	    else
		gsettings set org.gnome.desktop.input-sources xkb-options "['caps:ctrl_modifier', 'altwin:swap_lalt_lwin', 'ctrl:swap_rwin_rctl', 'shift:both_capslock', 'lv3:ralt_alt']"
	    fi
	    ;;
    esac

    gsettings set org.gnome.desktop.interface gtk-key-theme Emacs
    # stop Ctrl-. getting blocked
    gsettings set org.freedesktop.ibus.panel.emoji hotkey "[]"

    # disable dock app launch keys
    gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-1 "[]"
    gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-2 "[]"
    gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-3 "[]"
    gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-4 "[]"

    # launchers
    gsettings set org.gnome.settings-daemon.plugins.media-keys terminal "['<Super>Return']"
    gsettings set org.gnome.settings-daemon.plugins.media-keys www "['<Super>w']"

    # screenshots
    gsettings set org.gnome.shell.keybindings screenshot "['<Super>p']"
    gsettings set org.gnome.shell.keybindings show-screenshot-ui "['<Shift><Super>p']"

    # navigation
    gsettings set org.gnome.desktop.wm.keybindings show-desktop '[]' # remove ctrl+alt+d
    gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-1 "['<Super>1']"
    gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-2 "['<Super>2']"
    gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-3 "['<Super>3']"
    gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-4 "['<Super>4']"
    gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-1 "['<Alt>1']"
    gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-2 "['<Alt>2']"
    gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-3 "['<Alt>3']"
    gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-4 "['<Alt>4']"

    # typing
    gsettings set org.gnome.desktop.wm.keybindings switch-input-source "['<Super>space']"

    # windows
    gsettings set org.gnome.desktop.wm.keybindings activate-window-menu "['<Alt><Super>space']" # instead of alt+space

    # custom shortcuts

    name=()
    command=()
    binding=()
    if [[ -x ~/tools/bin/emacs ]]; then
	name+=( "emacs" )
	command+=( ~/tools/bin/emacs )
	binding+=( '<Super>e' )
    fi
    if [[ -x ~/dev/macDict/macDict.sh ]]; then
	name+=( "macDict" )
	command+=( ~/dev/macDict/macDict.sh )
	binding+=( '<Super>d' )
    fi

    if [[ "$(hostname)" = "goose" ]]; then
	# disable screen brightness keys with default step of 5 (requires session restart)
	gsettings set org.gnome.settings-daemon.plugins.media-keys screen-brightness-down-static "['']"
	gsettings set org.gnome.settings-daemon.plugins.media-keys screen-brightness-up-static "['']"

	# smaller brightness step
	name+=( "brightessUp" )
	command+=( "${HOME}/dotfiles-public/bin/brightness.sh 1" )
	binding+=( 'MonBrightnessUp' )
	name+=( "brightessDown" )
	command+=( "${HOME}/dotfiles-public/bin/brightness.sh -1" )
	binding+=( 'MonBrightnessDown' )
    fi

    len=${#name[@]}
    key=org.gnome.settings-daemon.plugins.media-keys.custom-keybinding
    custom=""
    for (( i=0; i<len; i++ )); do
	if (( $i > 0 )); then
            custom="${custom},"
	fi
	path="/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom${i}/"
	gsettings set "${key}:${path}" name "${name[$i]}"
	gsettings set "${key}:${path}" command "${command[$i]}"
	gsettings set "${key}:${path}" binding "${binding[$i]}"
	custom="${custom}'${path}'"
    done
    gsettings set org.gnome.settings-daemon.plugins.media-keys custom-keybindings "[${custom}]"

fi

if [[ "$XDG_CURRENT_DESKTOP" = "MATE" ]]; then
    gsettings set org.mate.interface gtk-key-theme Emacs
    # disable Super_L key opening menu
    gsettings set org.mate.mate-menu hot-key ''
    gsettings set com.solus-project.brisk-menu hot-key ''
    # disable Super_L-E opening file explorer
    gsettings set org.mate.Marco.global-keybindings run-command-3 ''
fi

if [[ "$(hostname)" = "asusbox" ]]; then
    setxkbmap -option # back to default
    setxkbmap -option caps:ctrl_modifier
    setxkbmap -option ctrl:ralt_rctrl
    setxkbmap -option altwin:menu_win
    setxkbmap -option shift:both_capslock

    for id in $(xinput list | sed -e '1,/Virtual core keyboard/d' | grep -e 'Keychron' -e 'PCoIP.*Keyboard' -e 'RGS keyboard' | sed -e 's/.*id=\([0-9]\+\).*/\1/')
    do
	setxkbmap -device "${id}" -option
	setxkbmap -device "${id}" -option caps:ctrl_modifier
	setxkbmap -device "${id}" -option ctrl:swap_rwin_rctl
	setxkbmap -device "${id}" -option altwin:swap_lalt_lwin
	setxkbmap -device "${id}" -option shift:both_capslock
    done
fi

# https://docs.microsoft.com/en-us/sysinternals/downloads/ctrl2cap
# https://github.com/microsoft/PowerToys/releases/

# Win+G opens "Game bar". To remove it:
# Start, search "Windows PowerShell (Admin)"
# Get-AppxPackage Microsoft.XboxGamingOverlay | Remove-AppxPackage

