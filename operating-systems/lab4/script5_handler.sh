val=1
op='+' 
tail -n 0 -f data.txt | while true 
do
	read line
	# echo $line
	if [[ "$line" == "+" ]]; then
		op="+"
		echo "add mode on"
	elif [[ "$line" == "*" ]]; then
		op="*"
		echo "multiply mode on"
	elif [[ "$line" =~ [0-9]+ ]]; then
		val=$((val $op line))
		echo "val = $val"
	elif [[ "$line" == "QUIT" ]]; then
		break
	else 
		exit 1
	fi
done