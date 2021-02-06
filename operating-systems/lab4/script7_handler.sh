echo $$ > .pid1
val=1
work=1
op="+"

usr1()
{
	op="+"
}

usr2() 
{
	op="*"
}

term() 
{
	work=0
}

trap 'usr1' USR1
trap 'usr2' USR2
trap 'term' SIGTERM

while true;
do
	if [[ "$work" == "0" ]]; then
		echo "Stopped by SIGTERM"
		exit
	else
		val=$((val $op 2))
		echo $val
	fi
	sleep 1
done
