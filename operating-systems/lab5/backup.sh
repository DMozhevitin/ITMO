weekago=$(date -d 'now -7 days' +%F)
curdate=$(date +%F)
report="$HOME/backup-report"
source=$HOME/source
dir=""

for file in $(echo $(ls "$HOME" | grep "Backup-"))
do
	date=$(echo $file | awk -F "Backup-" '{ print $2 }')
	if [[ "$date" > "$weekago" ]]; then
		dir=$file
		break
	fi
done 

if [[ "$dir" == "" ]]; then
	dir="$HOME/Backup-$curdate"

	mkdir "$HOME/Backup-$curdate" && echo "Backup-$curdate has been created" >> "$report"

	for file in $source/*
	do
		cp "$file" "$dir"
		echo "$file has been copied" >> "$report"
	done

else
	for file in $source/*
	do
		f="$HOME/$dir/$(basename $file)"
		if [[ -e $f ]]; then
			size1=$(stat --printf="%s" $file)
			size2=$(stat --printf="%s" $f)
			if (($size1 != size2)); then
				mv "$f" $f.$curdate
				cp "$file" "$HOME/$dir"
				echo "$file has been copied" >> "$report"
				echo "$f -> $f.$curdate" >> "$report" 
			fi
		fi
	done
fi