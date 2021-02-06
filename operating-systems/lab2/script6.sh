wc -l $(find /var/log -name "*.log") 
	| tail -1 
	| awk -F" " '{ print $1} ' 