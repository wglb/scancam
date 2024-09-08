#!/bin/bash
for i in *.lsp
do
	cp -vfb $i ${1?Oops}
done
