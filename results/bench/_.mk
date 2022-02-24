$(DONE): \
	$(DIR)/lscpu \
	$(DIR)/json \
	$(DIR)/angstrom \
	$(DIR)/angstrom-mod

$(DIR)/lscpu:
	lscpu > $@
