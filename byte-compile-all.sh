#!/bin/sh

find . -name "*.el" -exec /Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs --batch --eval "(byte-compile-file \"{}\")" \;