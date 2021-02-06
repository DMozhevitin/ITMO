#!/bin/bash

echo 1: run nano
echo 2: run vim
echo 3: run links
echo 4: exit

read input

case $input in
	1)
		nano
		;;
	2)
		vim
		;;
	3)
		links
		;;
esac

