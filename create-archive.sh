#!/bin/bash -e
mkdir thesis-package
rm -rf thesis-package.tar.gz thesis-package/*

(cd prototype && git archive --prefix=prototype/ -o ../thesis-package/prototype.tar HEAD)
(cd prototype/lib/php-activerecord && git archive --prefix=php-activerecord/ -o ../../../thesis-package/php-activerecord.tar HEAD)
(cd Comb && git archive --prefix=Comb/ -o ../thesis-package/comb.tar HEAD)

cd thesis-package
tar -xf prototype.tar
tar -xf comb.tar

cd Comb
rm Editor/app/scripts/comb
cp -r parser Editor/app/scripts/comb

rm MovieDatabase/client/scripts/vendor/comb
cp -r parser MovieDatabase/client/scripts/vendor/comb

cp -r ../../Comb/MovieDatabase/server/vendor MovieDatabase/server

cd ..

cd prototype/lib
tar -xf ../../php-activerecord.tar
cd ../..

rm *.tar
cd ..

cp thesis/thesis.*.pdf thesis-package/
tar --exclude=.DS_Store -cf thesis-package.tar.gz thesis-package
sha1sum thesis-package.tar.gz
