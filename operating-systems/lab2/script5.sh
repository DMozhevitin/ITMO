awk -F":" '{ print $1 ":" $3 }' /etc/passwd 
	| sort -k 2