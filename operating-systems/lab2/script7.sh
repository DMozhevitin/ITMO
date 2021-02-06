man bash 
	| sed -e 's/[^[:alpha:]]/ /g' 
	| tr '\n' ' ' 
	| tr -s ' ' 
	| tr ' ' '\n' 
	| tr 'A-Z' 'a-z' 
	| grep -E "...." 
	| sort 
	| uniq -c 
	| sort -nr  
	| head -3