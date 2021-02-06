while true; do
	read line
	if [[ "$line" == "TERM" ]]; then
		kill -SIGTERM $(cat .pid)
		exit
	fi
done