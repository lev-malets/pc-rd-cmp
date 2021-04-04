$(eval $(base))

$(DONE): $(patsubst %/_.mk,$(TMP)/%/done,$(SUB))
	$(touch_target)

include $(SUB)
