all: get-deps compile ling-build-image

ling-build-image compile get-deps:
	rebar $@
