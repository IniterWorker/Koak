#!/usr/bin/env bash

sudo docker run -v $(pwd):/koak -e USERID=$UID -it arignir/llvm_debug
