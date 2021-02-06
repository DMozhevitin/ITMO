files=$(ls /sbin)
for file in $(echo -e "$files")
do
	ps aux | grep $file | awk -F " " ' { print $2 } '
done