#!/bin/bash
#
# Installs symlinks to the emacs configuration files in your home directory.

src=`pwd`

# Link the .emacs file.
ln -s $src/.emacs ~/.emacs

# Create the .emacs.d directory if it doesn't exist already.
if [ ! -d ~/.emacs.d ]; then
  mkdir -p ~/.emacs.d
fi

# Link the .emacs.d files.
for file in `ls $src/.emacs.d`; do
  ln -s $src/.emacs.d/$file ~/.emacs.d/$file
done
