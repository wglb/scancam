#!/bin/sh
sbcl --eval '(ql:quickload (list :cl-html-parse :com.google.flag :flexi-streams :cl-base64 :quri :dexador :cl-gd :iolib/sockets :osicat :com.google.flag :str))'

#
# :tokens
# :move-files
# :configuration-r
# :lock-and-trigger
# :dex-get
