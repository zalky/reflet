.PHONY: npm cljs nuke clean

version-number  = 0.2.0
group-id        = io.zalky
artifact-id     = reflet
description     = A Re-frame/Reagent based library for frontend state management in Clojurescript
license         = :apache
url             = https://github.com/zalky/reflet

include make-clj/Makefile

.makecache/npm-install: package.json
	npm install
	@mkdir -p .makecache
	@touch .makecache/npm-install

npm: .makecache/npm-install
	@:

cljs: npm
	clojure -M:cljs/client release app

nuke:
	@make nuke-super
	@rm -rf .makecache
	@rm -rf node_modules
	@rm -rf .shadow-cljs

clean:
	@echo "Cleaning target and resources"
	@rm -rf target
	@find resources -not -path "resources/audio*" -not -path "resources/client.css*" -not -name index.html  -mindepth 1 -prune -exec rm -rf {} \;

