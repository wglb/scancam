#!/bin/bash
buildapp --output scancam-built --asdf-tree ~/lisplib/public/ --asdf-tree ~/quicklisp/dists/quicklisp --asdf-tree /home/wgl/lisplib/lisp-lib --require sb-posix --compress-core --load-system scancam
