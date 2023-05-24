#!/bin/sh
git submodule init
git submodule update
ln -s $HOME/dotfiles/zsh/.zshrc $HOME/.zshrc || true
