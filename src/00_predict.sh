#!/bin/bash
# Run live recognition, play file and output to txt (smile.log)
# Usage: 00_predict.sh "file.wav"
trap "exit" INT TERM
trap "kill 0" EXIT

file="$1" 
./SMILExtract -C config/emobase_live4.conf &
sleep 7.5; cvlc --play-and-exit "$file"
sleep 7.5

exit
