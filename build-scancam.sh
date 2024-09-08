#!/bin/sh
#build the sucker.
sbcl  --dynamic-space-size 2000  --disable-debugger --eval "(asdf:operate 'asdf:load-op 'scancam)" --eval '(in-package #:scancam)' --eval '(save-core)'
