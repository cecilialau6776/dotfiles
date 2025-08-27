#!/bin/sh

# init submodules, mostly for zshrc
git submodule init
git submodule update

# make sure required directories exist
mkdir -p $HOME/.config/mpd
mkdir -p $HOME/.config/mpd/playlists
mkdir -p $HOME/.config/ncmpcpp
mkdir -p $HOME/.config/ncmpcpp/lyrics

# make symlinks
ln -s $HOME/dotfiles/zsh/.zshrc $HOME/.zshrc
ln -s $HOME/dotfiles/.tmux.conf $HOME/.tmux.conf
ln -s $HOME/dotfiles/.emacs.d $HOME/.emacs.d
ln -s $HOME/dotfiles/git/gitconfig $HOME/.gitconfig
ln -s $HOME/dotfiles/mpd/mpd.conf $HOME/.config/mpd/mpd.conf
ln -s $HOME/dotfiles/ncmpcpp/bindings $HOME/.config/ncmpcpp/bindings

# define `xterm-24bit` for using emacs through ssh
/usr/bin/tic -x -o ~/.terminfo xterm-24bit.terminfo
