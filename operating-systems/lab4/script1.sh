mkdir ~/test && { 
		echo "catalog test was created successfully" >> ~/report
		touch ~/test/$(date +'%y-%m-%d_%H:%M:%S')
		}

ping -c 1 "www.net_nikogo.ru"  || echo "host is unavaliable" >> ~/report