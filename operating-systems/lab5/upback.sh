backupdir=""
source=$HOME/source
for file in $(echo $(ls "$HOME" | grep "Backup-"))
do
	date=$(echo $file | awk -F "Backup-" '{ print $2 }')
	if [[ "$date" > "$backupdir" ]]; then
		backupdir=$file
		break
	fi
done 

if [[ backupdir != "" ]]; then
	for file in $(ls "$HOME/$backupdir" | grep -v -E "*.[0-9]{4}-[0-9]{2}-[0-9]{2}") 
	do
		cp "$HOME/$backupdir/$file" "$source"
	done
fi
 