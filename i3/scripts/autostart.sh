~/.config/i3/polybar/polybar-i3 &

lxpolkit &
dunst -config ~/.config/i3/dunst/dunstrc &
picom --config ~/.config/i3/picom/picom.conf --animations -b &
feh --bg-fill $(cat ~/.config/default) &

# sxhkd
pkill -x sxhkd
sxhkd -c ~/.config/i3/sxhkd/sxhkdrc &
