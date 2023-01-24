SHELL=/usr/bin/env bash

EMACS ?= emacs
EASK ?= eask

build:
	$(EASK) install-deps
	$(EASK) package
	$(EASK) install

ci: clean build compile checkdoc lint

compile:
	@echo "Compiling..."
	$(EASK) compile

checkdoc:
	$(EASK) lint checkdoc

lint:
	@echo "package linting..."
	$(EASK) lint package

clean:
	$(EASK) clean-all

tag:
	$(eval TAG := $(filter-out $@,$(MAKECMDGOALS)))
	sed -i "s/;; Version: [0-9]\+.[0-9]\+.[0-9]\+/;; Version: $(TAG)/g" jet.el
	sed -i "s/jet-version-string \"[0-9]\+.[0-9]\+.[0-9]\+\"/jet-version-string \"$(TAG)\"/g" jet.el
	sed -i "s/\"jet\" \"[0-9]\+.[0-9]\+.[0-9]\+\"/\"jet\" \"$(TAG)\"/g" Eask
	git add jet.el Eask
	git commit -m "Bump jet: $(TAG)"
	git tag $(TAG)
	git push origin HEAD
	git push origin --tags

# Allow args to make commands
%:
	@:

.PHONY : ci compile checkdoc lint clean tag
