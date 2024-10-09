#!/bin/bash

# run ledger on encrypted file

# edit arguments, replacing file with stdin
new_args=()
skip=0
file=
for a in "$@"; do
    if [[ "${a}" == "-f" ]]; then
        new_args+=("-f")
        new_args+=("-")
        skip=2
    fi
    if (( "${skip}" )); then
        if (( ${skip} == 1 )); then
            file="${a}"
        fi
        skip=$(( ${skip} - 1 ));
    else
        new_args+=("${a}")
    fi
done

if [[ -z "${file}" ]]; then
    echo "$0 : failed to find file in argument list" 1>&2
    exit 1
fi

# set -xe
if [[ "${file##*.}" == "gpg" ]]; then
    gpg --decrypt "${file}" 2> /dev/null | ledger "${new_args[@]}"
else
    ledger "${@}"
fi
