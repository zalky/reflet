.PHONY: npm nuke clean

version-number  = 0.2.0
group-id        = io.zalky
artifact-id     = reflet
description     = A Re-frame/Reagent based library for frontend state management in Clojurescript
license         = :apache

include make-clj/Makefile

.makecache/npm-install: package.json
	npm install
	@mkdir -p .makecache
	@touch .makecache/npm-install

npm: .makecache/npm-install
	@:

nuke:
	@make nuke-super
	@rm -rf .makecache
	@rm -rf node_modules
	@rm -rf .shadow-cljs

clean:
	@echo "Cleaning target and resources"
	@rm -rf target
	@find resources -not -path "resources/audio*" -not -name index.html  -mindepth 1 -prune -exec rm -rf {} \;
