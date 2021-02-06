while (<>) {
	s/\b([123456789]\d*)0\b/$1/g;
	print;
}