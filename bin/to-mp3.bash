#!/bin/bash

if [[ "$#" -lt 1 ]]; then
    echo "usage: $(basename $0) file.mkv [start]"
    exit 1
fi

start=
if [[ "$#" -eq 2 ]]; then
    start="-ss $2"
fi

mp3="${1%.*}.mp3"
if [[ -f "${mp3}" ]]; then
    echo "Stopping, ${mp3} already exists";
    exit 1
fi

set -xe

ffmpeg ${start} -i "${1}" -vn -c:a mp3 "${mp3}"
