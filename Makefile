include Makefile.base

.PHONY: demo
demo: build
	stack exec -- linenoise-demo

.PHONY: download
download:
	curl --output cbits/LICENSE https://raw.githubusercontent.com/antirez/linenoise/master/LICENSE
	curl --output cbits/linenoise.h https://raw.githubusercontent.com/antirez/linenoise/master/linenoise.h
	curl --output cbits/linenoise.c https://raw.githubusercontent.com/antirez/linenoise/master/linenoise.c
