grep -rh "ACPI" /var/log > errors.log
grep -h -E "(\/.+)+\.[[:alnum:]]+" errors.log