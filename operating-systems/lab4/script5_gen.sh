while true; do
	read line

	if [[ "$line" == "QUIT" ]]; then
		echo $line >> data.txt
		break
	fi

	if [[ "$line" == "*" ]]; then
		echo "*">> data.txt
	elif [[ "$line" == "+" || "$line" =~ [0-9]+ ]]; then
		echo $line >> data.txt
	else
		echo "wrong input"
		exit 1
	fi
done