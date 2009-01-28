#!/bin/sh

find . -name "*.el" -exec emacs --batch --eval "(byte-compile-file \"{}\")" \;