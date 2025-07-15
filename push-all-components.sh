#!/usr/bin/bash
cd ..
for i in xlog configuration-r scancam dex-wrap tokens move-files
do
	echo ---- $i ----
	pushd $i
	sh push-both.sh
	popd
done
