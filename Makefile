all: build lint

.PHONY: setup
setup:
	stack setup
	stack build --dependencies-only --no-run-tests
	stack install hlint weeder

.PHONY: build
build:
	stack build --pedantic --no-run-tests

.PHONY: lint
lint:
	hlint .
	weeder .

.PHONY: check-nightly
check-nightly:
	stack setup --resolver nightly
	stack build --resolver nightly --pedantic
