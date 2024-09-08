#!/bin/sh
#build the sucker.
sbcl --dynamic-space-size 2000  --disable-debugger --eval "(asdf:operate 'asdf:load-op 'dup-images-by-hash)" --eval '(in-package #:dup-images-by-hash)' --eval '(save-exe)' 
