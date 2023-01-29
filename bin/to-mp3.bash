#!/bin/bash

if [[ "$#" -ne 1 ]]; then
    echo "usage: $0 file.mkv"
    exit 1
fi

mp3="${1%.*}.mp3"
if [[ -f "${mp3}" ]]; then
    echo "Stopping, ${mp3} already exists";
    exit 1
fi

set -xe

ffmpeg -i "${1}" -vn -c:a mp3 "${mp3}"
