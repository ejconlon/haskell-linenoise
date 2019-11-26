.PHONY: cisetup
cisetup:
	stack setup

.PHONY: citest
citest: test

.PHONY: cidocs
cidocs: docs

.PHONY: cicompress
cicompress:
	tar -cvzf /tmp/haddock.tar.gz -C $$(stack path --local-install-root)/doc .

.PHONY: build
build:
	stack build

.PHONY: test
test:
	stack test

.PHONY: docs
docs:
	stack haddock --no-haddock-deps

.PHONY: demo
demo: build
	stack exec -- linenoise-demo

.PHONY: deps
deps:
	stack build --copy-compiler-tool ghcid hlint stylish-haskell

.PHONY: lint
lint:
	stack exec -- hlint -i 'Use newtype instead of data' src app

.PHONY: format
format:
	find . -name '*.hs' | xargs -t stack exec -- stylish-haskell -i

.PHONY: download
download:
	curl --output cbits/LICENSE https://raw.githubusercontent.com/antirez/linenoise/master/LICENSE
	curl --output cbits/linenoise.h https://raw.githubusercontent.com/antirez/linenoise/master/linenoise.h
	curl --output cbits/linenoise.c https://raw.githubusercontent.com/antirez/linenoise/master/linenoise.c
