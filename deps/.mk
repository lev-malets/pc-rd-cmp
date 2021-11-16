include make/base.mk

define clone
$(call args,repo/$1)
$(call arg,repo/$1,url,$2)
$(call arg,repo/$1,branch,$3)
$(call arg,repo/$1,commit,$4)
$(call arg,repo/$1,version,1)

$K/repo/$1: $K/args/repo/$1
	rm -rf $T/$1
	git clone $2 -b $3 $T/$1
	git -C $T/$1 config user.email '_'
	git -C $T/$1 config user.name '_'
	git -C $T/$1 reset $4 --hard
	mkdir -p $(dir $K/repo/$1)
	touch $K/repo/$1
endef

define patch
$K/patch/$1: $K/repo/$1 $2
	git -C $T/$1 reset $$(shell cat $(ARG)/repo/$1/commit) --hard
	patch -d $T/$1 -p1 < $2
	git -C $T/$1 add .
	git -C $T/$1 commit -m '_'
	mkdir -p $(dir $K/patch/$1)
	touch $K/patch/$1

$D/update_patch/$1: $K/repo/$1
	git -C $T/$1 diff $$(shell cat $(ARG)/repo/$1/commit) > $2

.PHONY: $D/update_patch/$1
endef

define cmd
$(call clone,angstrom,'https://github.com/inhabitedtype/angstrom',master,b0e7849ec59746b44753a4c5f5954ba95d1c6c5a)
$(call patch,angstrom,$D/angstrom.patch)

$(call clone,bark,'https://github.com/justinlubin/bark',master,8586a2796361d54009c2f942e24fd5aad71f1cd0)
$(call patch,bark,$D/bark.patch)

$(call clone,mparser,'https://github.com/murmour/mparser',master,826fc1d9c68b5cafc0b2637ad395226f1a5abfa0)
$(call patch,mparser,$D/mparser.patch)

$(call clone,opal,'https://github.com/pyrocat101/opal',master,8d8a1f3d248610f5ce4890951ea0ffb1b143d39b)
$(call patch,opal,$D/opal.patch)

$(call clone,syntax,'https://github.com/rescript-lang/syntax',master,d9c44bb22892dc4e77334f064d8bccd119fecde9)
$(call patch,syntax,$D/syntax.patch)

$K/done: $K/patch/angstrom $K/patch/bark $K/patch/mparser $K/patch/opal $K/patch/syntax
endef

$(eval $(cmd))
