nrescancam: dispatch.lisp rescancam.lisp image-compare.lisp rescancam.asd rescancam-package.lisp ../dup-images-by-hash/dup-images-by-hash.asd ../dup-images-by-hash/dup-images-by-hash.lisp ../replace-all.asd ../replace-all/replace-all.lisp ../move-files/move-files.lisp ../move-files/move-files.asd ../lock-and-trigger/* 
	./build-nrescancam.sh

rescancam: rescancam.lisp image-compare.lisp rescancam.asd rescancam-package.lisp ../dup-images-by-hash/dup-images-by-hash.asd ../dup-images-by-hash/dup-images-by-hash.lisp ../replace-all/replace-all.asd ../replace-all/replace-all.lisp ../move-files/move-files.lisp ../move-files/move-files.asd ../lock-and-trigger/* 
	./build-rescancam.sh

bump: *.lisp
	cat  system-version.expr
	bump-version-number system-version.expr
	cat  system-version.expr
	touch bump

both: bump rescancam
	touch both

break-ensure: break-ensure.lisp break-ensure.asd
	./build-break-ensure.sh

force: 
	./build-rescancam.sh

install: ~/bin/nrescancam  # ~/bin/rescancam

~/bin/rescancam: rescancam
	cp -v rescancam ~/bin

~/bin/nrescancam: nrescancam
	cp -v nrescancam ~/bin
