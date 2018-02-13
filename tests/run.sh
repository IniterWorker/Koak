#!/bin/bash

TEST=$(dirname "$0")/

pushd $TEST
python3 -m unittest discover -s . -t . -p "test_*" -v
popd