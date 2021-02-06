log=$(sed 's/(WW) /Warning:/g ' /var/log/Xorg.1.log 
	| sed 's/(II) /Information: /g' 
	| sed 's/(EE) /Information: /g')
echo "$log" | grep "Error:" > full.log
echo "$log" | grep "Warning:" >> full.log
cat full.log
