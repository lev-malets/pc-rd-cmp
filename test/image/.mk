$K/builded: force
	mkdir -p $(dir $@)
	docker build --tag $(shell cat $D/tag) $D
	docker build -q $D > $@.tmp
	test -f $@ && diff $@.tmp $@ > /dev/null || mv $@.tmp $@

$D/build: $K/builded

$D/delete-old: $D/tag
	docker images \
		| grep $(shell cat $< | awk -F: '{print $$1}') \
		| grep -v $(shell cat $< | awk -F: '{print $$2}') \
		| awk '{print $$3}' \
		| xargs -r docker image rm

$D/push: $K/loaded
	docker push $(shell cat $K/name)

$D/run: $D/tag
	bash $D/run.sh $<

.PHONY: $D/build $D/push $D/run $D/delete-old
