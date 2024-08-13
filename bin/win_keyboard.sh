#!/bin/bash

# Copy PowerToys settings for Apple or Dell keyboards

if [[ $(uname | cut -d- -f1) != "CYGWIN_NT" ]]; then
    echo "Not running on Cygwin" 1>&2
    exit 1
fi
if [[ "$#" -ne 1 ]]; then
    echo "usage: $(basename $0) <apple|dell>" 1>&2
    exit 1
fi

src_json="${HOME}/dotfiles-public/win/PowerToys/KeyboardManager/${1}_default.json"
dst_json="${HOMEPATH}/AppData/Local/Microsoft/PowerToys/Keyboard Manager/default.json"

if [[ ! -f "${src_json}" ]]; then
    echo "${src_json} does not exist" 1>&2
    exit 1
fi

cp -v "${src_json}" "${dst_json}"

echo
echo "Open PowerToys Keyboard Manager, click Remap a Key -> OK, to apply the settings"

pt="${HOMEPATH}/AppData/Local/PowerToys/PowerToys.exe"
set -xe
exec ${pt}
