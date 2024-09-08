scancam: dispatch.lisp scancam.lisp image-compare.lisp scancam.asd scancam-package.lisp dup-images-by-hash/dup-images-by-hash.asd dup-images-by-hash/dup-images-by-hash.lisp  
	./build-scancam.sh

xscancam: scancam.lisp image-compare.lisp scancam.asd scancam-package.lisp ../dup-images-by-hash/dup-images-by-hash.asd ../dup-images-by-hash/dup-images-by-hash.lisp ../replace-all/replace-all.asd ../replace-all/replace-all.lisp ../move-files/move-files.lisp ../move-files/move-files.asd ../lock-and-trigger/* 
	./build-scancam.sh

bump: *.lisp
	cat  system-version.expr
	bump-version-number system-version.expr
	cat  system-version.expr
	touch bump

both: bump scancam
	touch both

break-ensure: break-ensure.lisp break-ensure.asd
	./build-break-ensure.sh

force: 
	./build-scancam.sh

install: ~/bin/scancam  # ~/bin/scancam

~/bin/scancam: scancam
	cp -v scancam ~/bin

