echo $$ > .pid
work=1
x=0

term() 
{
	work=0
}

trap 'term' SIGTERM
while true;
do
	if [[ "$work" == "1" ]]; then
		x=$((x+1))
	else
		echo "Stopped by SIGTERM"
		exit
	fi
	sleep 1
done
