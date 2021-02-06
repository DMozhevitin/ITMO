#TODO: for i in {1..MAX_PPID_FROM_proc.out}
for i in {1..32768}
do
	avg=0
	cnt=$(grep "Parent_ProcessID=$i" proc.out | wc -l)
	echo $(grep "Parent_ProcessID=$i" proc.out)
	avgs=$(grep "Parent_ProcessID=$i" proc.out  |  awk -F "Average_Sleeping_Time=" ' { print $2 } ')
	for j in $avgs
	do
		avg=$(echo "$avg + $j" | bc -l)
	done
	if [ "$cnt" -ne 0 ]
	then
		echo Average_Sleeping_Children_of_ParentID=$i is $(echo "$avg / $cnt" | bc -l)
	fi
done > proc1.out