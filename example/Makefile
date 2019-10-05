SHELL := /bin/bash

PROJECT_NAME := javascript-extras-test

include ghcjs.mk

.PHONY: clean install start

clean::
	rm -rf node_modules

install: build
	npm install

start: | install
	node_modules/.bin/webpack-dev-server --content-base public
