#!/bin/bash

timeout_time=${1:-"10s"}
intbtweentrans=${2:-1}
urltoping=${3:-"www.google.com"}
name_file="test_$(date +"%Y_%m_%d__%H%M")"

echo "Total Arguments:" $#
echo "All Arguments values:" $@

echo "First->"  $1
echo "Second->" $2
echo "Third->"  $3

echo $name_file


sudo timeout $timeout_time ping -i $intbtweentrans $urltoping  | 
while read pong; do   
  echo "$(date +"%T.%N"): $pong"
done > "../data/raw/$name_file.txt"

./bash_script_v2.r "$name_file.txt"

xdg-open "../outputs/$name_file.pdf"

