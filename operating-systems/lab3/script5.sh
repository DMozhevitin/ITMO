files=$(ls /proc)

for file in $(echo "$files" | grep '[0-9]')
do
	PID=$(grep "^Pid:" "/proc/$file/status" | awk -F " " ' { print $2 }')
	PPID=$(grep "^PPid:" "/proc/$file/status" | awk -F " " ' { print $2 }')
	EXEC=$(grep "se.sum_exec_runtime" "/proc/$file/sched" | awk -F " " ' { print $3 } ')
	SWITCHES=$(grep "nr_switches" "/proc/$file/sched"  | awk -F " " ' { print $3 } ')
	SleepAVG="undefined"
	if [ "$SWITCHES" -ne 0 ]
	then
		SleepAVG=$(echo "$EXEC/$SWITCHES" | bc -l)
	fi
	echo "ProcessID=$PID : Parent_ProcessID=$PPID : Average_Sleeping_Time=$SleepAVG" >> proc.out
done
