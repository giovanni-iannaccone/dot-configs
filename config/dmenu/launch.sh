#!/bin/sh

APPS=$(
    find /usr/share/applications ~/.local/share/applications \
        -name '*.desktop' -type f 2>/dev/null |
    while read -r file; do
        name=$(grep -m1 '^Name=' "$file" | cut -d= -f2-)
        [ -n "$name" ] && printf '%s\t%s\n' "$name" "$file"
    done |
    sort -f |
    cut -f1 |
    dmenu \
        -i \
        -fn 'JetBrainsMono Nerd Font-12' \
        -nb '#000000' \
        -nf '#F2F4F8' \
        -sb '#4C7899' \
        -sf '#FFFFFF' \
        -p 'Apps:'
)

[ -n "$APPS" ] || exit 0

find /usr/share/applications ~/.local/share/applications \
    -name '*.desktop' -type f 2>/dev/null |
while read -r file; do
    name=$(grep -m1 '^Name=' "$file" | cut -d= -f2-)

    if [ "$name" = "$APPS" ]; then
        gtk-launch "$(basename "$file" .desktop)"
        exit 0
    fi
done
