# regex from https://www.shellhacks.com/ru/regex-find-email-addresses-file-grep/
grep -rho -E '\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,6}\b' /etc 
	| grep @ 
	| sort 
	| uniq 
	| tr '\n' ',' 
	| sed 's/.$//' > emails.list