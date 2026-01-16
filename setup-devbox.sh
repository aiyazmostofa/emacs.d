#!/bin/sh
set -e
cd ~

# Packages
sudo pacman -Syu --noconfirm
sudo pacman -S emacs-wayland git nix ttf-jetbrains-mono direnv --noconfirm

# Nix
mkdir -p .config/nix
echo 'experimental-features = nix-command flakes' >> .config/nix/nix.conf
sudo systemctl enable nix-daemon
sudo systemctl start nix-daemon

# Direnv
echo 'eval "$(direnv hook bash)"' >> .bashrc

# Symlinks
ln -s /home/aiyaz/.config/emacs .emacs.d
ln -s /home/aiyaz/.ssh .ssh
ln -s /home/aiyaz/.config/git/config .gitconfig
