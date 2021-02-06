#!/bin/bash

read n
cnt=1

while (($n % 2 != 0)); do
	((cnt++))
	read n
done

echo $cnt