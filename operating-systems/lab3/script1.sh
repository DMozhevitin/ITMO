ps | wc -l
ps | tr -s " " | awk -F " " '{ print $1 " " $4 }' 