#!/bin/bash

if [[ "$XDG_CURRENT_DESKTOP" = "ubuntu:GNOME" ]]; then

    # previously in /etc/default/keyboard; man keyboard; sudo dpkg-reconfigure keyboard-configuration
    # On hedgehog
    # XKBOPTIONS="caps:ctrl_modifier,ctrl:ralt_rctrl,ctrl:rctrl_ralt,shift:both_capslock"
    # On goose
    # XKBOPTIONS="caps:ctrl_modifier,altwin:swap_lalt_lwin,ctrl:swap_rwin_rctl,shift:both_capslock"

    # system
    gsettings set org.gnome.shell.keybindings toggle-overview "['LaunchA']"
    # stop Super_L tap showing window overview
    gsettings set org.gnome.mutter overlay-key ''

    case "$(hostname -s)" in
	"goose")
	    gsettings set org.gnome.desktop.input-sources xkb-options "['ctrl:swapcaps', 'altwin:swap_lalt_lwin', 'ctrl:swap_rwin_rctl', 'lv3:ralt_alt']"
	    # gsettings set org.gnome.desktop.input-sources xkb-options "['caps:swapescape', 'altwin:swap_lalt_lwin', 'ctrl:swap_rwin_rctl', 'lv3:ralt_alt']"
	    # gsettings set org.gnome.desktop.input-sources xkb-options "['caps:escape_shifted_capslock', 'ctrl:swap_lalt_lctl_lwin', 'ctrl:swap_rwin_rctl']"
	    ;;

	"hedgehog")
        if [[ -n $(xinput list --name-only | grep -i 'Apple.*Magic Keyboard') ]]; then
            # external apple keyboard
            gsettings set org.gnome.desktop.input-sources xkb-options "['ctrl:swapcaps', 'altwin:swap_lalt_lwin', 'ctrl:swap_rwin_rctl', 'lv3:ralt_alt']"
        else
            # laptop keyboard
            gsettings set org.gnome.desktop.input-sources xkb-options "['ctrl:swapcaps', 'ctrl:ralt_rctrl', 'ctrl:rctrl_ralt']"
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
    gsettings set org.gnome.shell.keybindings switch-to-application-1 "[]"
    gsettings set org.gnome.shell.keybindings switch-to-application-2 "[]"
    gsettings set org.gnome.shell.keybindings switch-to-application-3 "[]"
    gsettings set org.gnome.shell.keybindings switch-to-application-4 "[]"

    # launchers
    gsettings set org.gnome.settings-daemon.plugins.media-keys terminal "['<Super>Return']"
    gsettings set org.gnome.settings-daemon.plugins.media-keys www "['<Super>w']"

    # screenshots
    gsettings set org.gnome.shell.keybindings screenshot "['<Super>p']"
    gsettings set org.gnome.shell.keybindings show-screenshot-ui "['<Shift><Super>p']"

    # navigation
    gsettings set org.gnome.desktop.wm.keybindings show-desktop "['<Super>g']" # want ctrl+alt+d for emacs
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

    # gnome-terminal
    gsettings set org.gnome.Terminal.Legacy.Keybindings:/org/gnome/terminal/legacy/keybindings/ next-tab "'<Alt>braceright'"
    gsettings set org.gnome.Terminal.Legacy.Keybindings:/org/gnome/terminal/legacy/keybindings/ prev-tab "'<Alt>braceleft'"
    gsettings set org.gnome.Terminal.Legacy.Keybindings:/org/gnome/terminal/legacy/keybindings/ close-tab "'<Alt>w'"
    gsettings set org.gnome.Terminal.Legacy.Keybindings:/org/gnome/terminal/legacy/keybindings/ new-tab "'<Alt>t'"


    # custom shortcuts

    name=()
    command=()
    binding=()
    if [[ -x /usr/bin/nautilus ]]; then
	name+=( "nautilus" )
	command+=( nautilus )
	binding+=( '<Super>f' )
    fi
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
    if command -v kitty > /dev/null 2>&1; then
	name+=( "kitty" )
	command+=( kitty )
	binding+=( '<Super>k' )
    fi

    if [[ "$(hostname -s)" = "goose" ]]; then
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

if [[ "$(hostname -s)" = "asusbox" ]]; then
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

# PowerToys, Keyboard Manager, Remap a key. Caps to Ctrl.
# PowerToys, Mouse utilities, Enable Find My Mouse = OFF
# Apple keyboard:
#     Remap keys:
#         Win Right -> Ctrl Right
#         Alt Right -> Ctrl Right
#         Win Left -> Alt Left
#         Alt Left -> Win Left
#         Caps Lock -> Ctrl Left
#         F12 -> Caps Lock
#     Remap a shortcut:
#         Alt Left + B             -> Alt Left + Shift Left + B
#         Ctrl Left + Alt Left + B -> Ctrl Left + Alt Left + Shift Left + B
#             Workaround for Win+B blocked
#             Disabled shift-select-mode in Emacs to avoid the shift taking effect.
#
# PowerToys stores these in a default.json.
# See win_keyboard.sh for Apple/Dell-specific remappings.

# Win+Shift+L still registers as Alt+L (downcase) in Emacs after the Win/Alt remapping.
#
# Can't have Win+L without shift, as it can't be remapped with PowerToys.
# Disabling the Win+L screenlock with regedit (run as Administrator) breaks the
# PowerToys settings:
#
#   - HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Policies\System
#   - If there is no System key, create one
#   - Create a 32bit DWORD, named DisableLockWorkstation
#   - Set value to 1
#   - It will take effect immediately

# Win11 Settings, Accessibility, Keyboard, Sticky Keys
# Lock shortcut keys when pressed twice in a row = OFF

# Win+G opens "Game bar". To remove it:
# Start, search "Windows PowerShell (Admin)"
# Get-AppxPackage Microsoft.XboxGamingOverlay | Remove-AppxPackage

# Tilde/backtick blocked, or double quote inserted twice. To fix:
# Start, Search "Settings"
# Time and Language
# Advanced keyboard settings
# Override for default input method = Use language list (recommended)
# Input language hotkeys, opens Text Services and Input Languages window
# Advanced Key settings
# Between input languages
# Change Key Sequence
# Switch Input Language = Not assigned
# Switch Keyboard layout = Ctrl + Shift
