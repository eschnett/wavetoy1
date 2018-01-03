#!/bin/sh

if [ "$#" != 2 ]; then
    echo "Synopsis: $0 <cmd> <file>" >&2
    exit 1
fi

cmd="$1"
file="$2"

"$cmd" "$file" >"$file.tmp"
if cmp "$file" "$file.tmp" >/dev/null; then
    rm -f "$file.tmp"
else
    mv "$file.tmp" "$file"
fi
