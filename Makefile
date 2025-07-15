scancam: dispatch.lisp scancam.lisp image-compare.lisp scancam.asd scancam-package.lisp dup-images-by-hash/dup-images-by-hash.asd dup-images-by-hash/dup-images-by-hash.lisp globals.lisp
	./build-scancam.sh

scancam-compressed: dispatch.lisp scancam.lisp image-compare.lisp scancam.asd scancam-package.lisp dup-images-by-hash/dup-images-by-hash.asd dup-images-by-hash/dup-images-by-hash.lisp globals.lisp 
	./build-scancam-compressed.sh

alternate-build: scancam
	buildapp --output scancam-built --asdf-tree ~/lisplib/public/ --asdf-tree ~/quicklisp/dists/quicklisp --asdf-tree /home/wgl/lisplib/lisp-lib --require sb-posix --compress-core --load-system scancam

xscancam: scancam.lisp image-compare.lisp scancam.asd scancam-package.lisp ../dup-images-by-hash/dup-images-by-hash.asd ../dup-images-by-hash/dup-images-by-hash.lisp ../replace-all/replace-all.asd ../replace-all/replace-all.lisp ../move-files/move-files.lisp ../move-files/move-files.asd ../lock-and-trigger/* 
	./build-scancam.sh

break-ensure: break-ensure.lisp break-ensure.asd
	./build-break-ensure.sh

force: 
	./build-scancam.sh

install: ~/bin/scancam  # ~/bin/scancam

~/bin/scancam: scancam
	cp -v scancam ~/bin

# An altern
