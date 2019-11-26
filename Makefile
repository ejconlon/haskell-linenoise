.PHONY: build
build:
	stack build

.PHONY: demo
demo: build
	stack exec -- linenoise-demo

.PHONY: deps
deps:
	stack build --copy-compiler-tool hlint

.PHONY: lint
lint:
	stack exec -- hlint -i 'Use newtype instead of data' src app
