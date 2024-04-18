#!/bin/sh

# init submodules, mostly for zshrc
git submodule init
git submodule update

# make sure required directories exist
mkdir -p $HOME/.config/mpd || true
mkdir -p $HOME/.config/mpd/playlists || true
mkdir -p $HOME/.config/ncmpcpp || true
mkdir -p $HOME/.config/ncmpcpp/lyrics || true

# make symlinks
ln -s $HOME/dotfiles/zsh/.zshrc $HOME/.zshrc || true
ln -s $HOME/dotfiles/.tmux.conf $HOME/.tmux.conf || true
ln -s $HOME/dotfiles/.emacs.d/ $HOME/.emacs.d || true
ln -s $HOME/dotfiles/git/gitconfig $HOME/.gitconfig || true
ln -s $HOME/dotfiles/mpd/mpd.conf $HOME/.config/mpd/mpd.conf || true
ln -s $HOME/dotfiles/ncmpcpp/bindings $HOME/.config/ncmpcpp/bindings || true

# define `xterm-24bit` for using emacs through ssh
/usr/bin/tic -x -o ~/.terminfo xterm-24bit.terminfo
