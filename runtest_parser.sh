#!/bin/bash

SUCC_FILES=./test/parser/*.0
ERR_FILES=./test/parser*.1

ERR_SUM="ERRORS: "

for sf in $SUCC_FILES
do
    echo "processing $sf ..."
    ./myce < $sf
    if [ $? -gt 0 ]; then
        echo "ERROR!"
        ERR_SUM="$ERR_SUM $sf"
    fi
done

for ef in $ERR_FILES
do
    echo "processing $ef ..."
    ./myce < $ef
    if [ $? -eq 0 ]; then
        echo "ERROR!"
        ERR_SUM="$ERR_SUM $ef"
    fi
done

echo "$ERR_SUM"

