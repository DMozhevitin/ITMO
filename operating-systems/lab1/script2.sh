#!/bin/bash

max="$1"

if (( $2 > $max)); then
	max=$2
fi

if (( $3 > $max)); then
	max=$3
fi

echo $max