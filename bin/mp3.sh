#!/bin/bash

if [[ "$#" -lt 1 ]]; then
    echo "usage: $(basename $0) file.mkv [start [stop]]"
    exit 1
fi

start=
if [[ "$#" -gt 1 ]]; then
    start="-ss $2"
fi
stop=
if [[ "$#" -gt 2 ]]; then
    stop="-to $3"
fi

mp3="${1%.*}.mp3"
if [[ -f "${mp3}" ]]; then
    echo "Stopping, ${mp3} already exists";
    exit 1
fi

set -xe

ffmpeg -i "${1}" ${start} ${stop} -vn -c:a mp3 "${mp3}"
