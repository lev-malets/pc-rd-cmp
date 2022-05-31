$K/name: force
	mkdir -p $K
	nix build .#test-image-name -o $T/image-name
	test -f $@ && diff $T/image-name $@ > /dev/null || cat $T/image-name > $@

$K/builded: $K/name
	nix build .#test-image -o $T/image
	touch $@

$K/loaded: $K/builded
	docker load -i $T/image
	touch $@

$D/name: $K/name
	@ echo $(shell cat $<)

$D/build: $K/builded

$D/load: $K/loaded

$D/delete-old: $K/name
	docker images \
		| grep $(shell cat $< | awk -F: '{print $$1}') \
		| grep -v $(shell cat $< | awk -F: '{print $$2}') \
		| awk '{print $$3}' \
		| xargs -r docker image rm

$D/push: $K/loaded
	docker push $(shell cat $K/name)

$D/run: $K/name
	bash $D/run.sh $K $T

.PHONY: $D/build $D/load $D/push $D/run $D/delete-old
