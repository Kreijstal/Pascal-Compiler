#!/bin/bash
set -e
INPUT_FILE=$1
/app/build/GPC/flatAST $INPUT_FILE
