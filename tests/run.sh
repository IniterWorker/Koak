#!/bin/bash

TEST=$(dirname "$0")/

pushd $TEST > /dev/null
python3 -m unittestcolor discover -s . -t . -p "test_*"
popd > /dev/null
