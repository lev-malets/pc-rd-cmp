include make/base.mk

define cmd
$D:
	dune exec $D/main.exe
endef

$(eval $(cmd))
