file=$1
trash=~/.trash

entries=$(grep $file ~/.trash.log)

for entry in $entries;
do
	rfile=$(echo $entry | awk -F ":" '{for (i=2; i<=NF; i++) printf $i ":"}')
	rfile=${rfile::-1}
	echo "would you like to restore this file?(Y/N)"
	read ans
	if [[ $ans == "Y" ]];
	then
		dir="$(dirname "$entry")"
		filename=$(echo $entry | awk -F ":" '{ print $2 }')

		if [[ !(-e "$dir") ]];
		then
			echo "couldn't find this directory, restoring into $HOME..."
			dir=$HOME
		fi

		{
			ln "$trash/$filename" "$file"
			rm "$trash/$filename"
		} && echo "file has been restored"

	elif [[ $ans != "N" ]]; then
		echo "incorrect answer"		
	fi
done
