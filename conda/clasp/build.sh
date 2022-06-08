mkdir -p $PREFIX/etc/conda/activate.d/
mkdir -p $PREFIX/etc/conda/deactivate.d/

cat >$PREFIX/etc/conda/activate.d/activate_clasp.sh <<EOL
#!/bin/sh
export CLASP_HOME=\$CONDA_PREFIX/share/clasp/
EOL

cat >$PREFIX/etc/conda/deactivate.d/deactivate_clasp.sh <<EOL
#!/bin/sh
unset CLASP_HOME
EOL

chmod u+x $PREFIX/etc/conda/activate.d/activate_clasp.sh
chmod u+x $PREFIX/etc/conda/deactivate.d/deactivate_clasp.sh

./koga --skip-sync=ansi-test,mps --bin-path=$PREFIX/bin/ --share-path=$PREFIX/share/clasp/ --lib-path=$PREFIX/share/clasp/
ninja -C build install
