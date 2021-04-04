$(eval $(base))

$(DIR)/json: $(TMP)/src/json/bench/all
	cp $< $@
