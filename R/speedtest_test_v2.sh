#!/bin/bash

runtime=${1:-"2 minute"}
endtime=$(date -ud "$runtime" +%s)
name_file="speedtest_$(date +"%Y_%m_%d__%H%M")"

while [[ $(date -u +%s) -le $endtime ]]
do
    speedtest --json >>"$name_file.txt"
done
