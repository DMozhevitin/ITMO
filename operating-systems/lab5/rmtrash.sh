file=$1
trash=~/.trash

if [[ !(-e "$trash") ]] 
then
	mkdir ~/.trash
fi

nextsrc=".next"
if [[ !(-e $nextsrc) ]]
then
	touch "$nextsrc"
	echo 0 > $nextsrc
fi

nxt=$(cat $nextsrc)
nxt=$((nxt+1))
echo $nxt > $nextsrc

ln $file "$trash/$nxt"
rm $file

fullpath=$(readlink -f $file)

log=~/.trash.log
echo "$fullpath:$nxt" >> $log
echo "$nxt:$fullpath" >> $log 






