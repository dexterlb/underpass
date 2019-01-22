#!/bin/bash

echo "downloading WordNet database..."
wget http://wordnetcode.princeton.edu/wn3.1.dict.tar.gz || exit 1
sha256sum -c <<< '3f7d8be8ef6ecc7167d39b10d66954ec734280b5bdcd57f7d9eafe429d11c22a  wn3.1.dict.tar.gz' \
    || exit 1
tar -xf wn3.1.dict.tar.gz || exit 1
