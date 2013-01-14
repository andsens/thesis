#!/bin/bash -xe
(cd prototype; git archive --format=tar.gz --prefix=prototype HEAD > ../thesis-package/prototype.tar.gz)
(cd Comb; git archive --format=tar.gz --prefix=prototype HEAD > ../thesis-package/comb.tar.gz)
cp thesis/thesis.*.pdf thesis-package/
tar -cvf thesis-package.tar.gz thesis-package
sha1sum thesis-package.tar.gz
