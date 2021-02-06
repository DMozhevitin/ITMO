pid=$(cat .pid1)
while true;
do
	read line
	if [[ "$line" == "+" ]]; then
		kill -USR1 $pid
	elif [[ "$line" == "*" ]]; then
		kill -USR2 $pid
	elif [[ "$line" == "TERM" ]]; then
		kill -SIGTERM $pid
		exit
	fi
done
