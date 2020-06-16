#!/bin/bash
for i in {1..4}; do
	speedtest --json >>"speedtest_output1.txt"
done
