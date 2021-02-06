INTERPRETER=$(file $(find /bin) 
	| grep -i  "shell script" 
	| tr -s " " 
	| awk -F ": " '{ print $2 }' 
	| awk -F "shell script"  '{print  $1 }' 
	| uniq -ci 
	| sort -nr 
	| head -1 
	| awk -F " " '{ print $2}')
file $(find /bin) 
	| grep -i $INTERPRETER 
	| awk -F ":" ' { print $1 }' 
