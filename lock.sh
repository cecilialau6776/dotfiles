#!/bin/sh

mpc pause
pactl set-sink-mute @DEFAULT_SINK@ 1

BLANK='#00000000'
CLEAR='#ffffff22'
DEFAULT='#e4a0e0cc'
TEXT='#ee00eeee'
WRONG='#d52a00bb'
VERIFYING='#bb00bbbb'

# nofork because xss-lock expects a non-forking command
i3lock \
    --nofork \
    --insidever-color=$VERIFYING \
    --ringver-color=$VERIFYING \
    --insidewrong-color=$CLEAR \
    --ringwrong-color=$WRONG \
    --inside-color=$BLANK \
    --ring-color=$DEFAULT \
    --line-color=$BLANK \
    --separator-color=$DEFAULT \
    --verif-text="" \
    --verif-color=$TEXT \
    --wrong-color=$TEXT \
    --time-color=$TEXT \
    --date-color=$TEXT \
    --layout-color=$TEXT \
    --keyhl-color=$WRONG \
    --bshl-color=$WRONG \
    --image=$HOME/dotfiles/wallpaper.png \
    --clock \
    --indicator \
    --time-str="%-H:%M:%S" \
    --date-str="%A, %Y-%m-%d"
