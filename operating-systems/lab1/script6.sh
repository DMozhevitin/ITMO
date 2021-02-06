#!/bin/bash


if [[ "$PWD" == "$HOME" ]]; then
	echo $PWD
	exit 0
fi

echo ERORR: This is not home directory 
exit 1
