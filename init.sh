#!/bin/sh

# init submodules, mostly for zshrc
git submodule init
git submodule update

# make symlinks
ln -s $HOME/dotfiles/zsh/.zshrc $HOME/.zshrc || true
ln -s $HOME/dotfiles/.tmux.conf $HOME/.tmux.conf || true
ln -s $HOME/dotfiles/.emacs.d/ $HOME/.emacs.d || true
ln -s $HOME/dotfiles/git/gitconfig $HOME/.gitconfig || true

# define `xterm-24bit` for using emacs through ssh
/usr/bin/tic -x -o ~/.terminfo xterm-24bit.terminfo
