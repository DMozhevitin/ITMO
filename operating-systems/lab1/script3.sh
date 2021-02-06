#!/bin/bash

str=""
ans=""
while [[ "${str}" != "q" ]]; do
	ans="${ans}${str}"
	read str
done

echo "$ans"