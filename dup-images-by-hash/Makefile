dup-images: dup-images-by-hash.lisp dup-images-by-hash.asd dup-images-by-hash-package.lisp 
	./build-dupe-images.sh
force: 
	./build-dupe-images.sh

install: ~/bin/dup-images

~/bin/dup-images: dup-images
	cp -v dup-images ~/bin
