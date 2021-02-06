$start = 1;
$end = 0;
$current_blank = 0;
$was_space = 0;

while (<>) {
	if (/\S/) {
		$start = 0;
	}

	if ($start == 0) {
		if ($current_blank == 1 && /^\s+$/) {
			print "";
		} else {
			if ($current_blank == 0 && /^\s+$/) {
				$current_blank = 1;
				$was_space = 1;
			} else {
				if ($was_space == 1) {
					$was_space = 0;
					print "\n";
				}

				$current_blank = 0;
				s/(^(\s)+)//g;
				s/((\s)+$)//g;
				s/\s\s+/ /g;
				print;
				print "\n";
			}
		}
	}	
}