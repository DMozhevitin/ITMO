files=$(ls /proc)
diffs=""
for file in $(echo "$files" | grep '[0-9]')
do
	diffs="${diffs}\n$file: $(cat "/proc/$file/statm" | awk -F " " ' { print ($2 - $3) } ')"
done

echo $diffs | sort -k2 -nr