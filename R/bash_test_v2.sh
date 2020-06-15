#!/bin/bash
name_file="test_$(date +"%Y_%m_%d__%H%M")"
echo $name_file
sudo timeout 120s ping -i 1 www.google.com  | 
while read pong; do   
  echo "$(date +"%T.%N"): $pong"
done > "../data/raw/$name_file.txt"

./bash_script_v2.r "$name_file.txt"

