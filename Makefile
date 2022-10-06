.PHONY: version tag push-tag npm jar install deploy test nuke clean

version-number  = 0.2.0
group-id        = io.zalky
artifact-id     = reflet
description     = A Re-frame/Reagent based library for frontend state management in Clojurescript
license         = apache

git-branch      := $(shell git rev-parse --abbrev-ref HEAD)
git-sha         := $(shell git rev-parse --short HEAD)
revision        := $(git-branch)-$(git-sha)
version         := $(version-number)-SNAPSHOT

project-config  := :lib $(group-id)/$(artifact-id) :version "\"$(version)\""
pom-config      := :description "\"$(description)\"" :license :$(license)

version:
	@echo "$(group-id)/$(artifact-id)-$(version)"

tag:
	@git tag $(version)
	@echo "Created tag $(version)"

push-tag:
	@git push origin $(version)

.makecache/npm-install: package.json
	npm install
	@mkdir -p .makecache
	@touch .makecache/npm-install

npm: .makecache/npm-install
	@:

jar: clean
	clojure -T:build jar $(project-config) $(pom-config)

target/$(artifact-id)-$(version).jar:
	@$(MAKE) jar

install: target/$(artifact-id)-$(version).jar
	clojure -T:build install $(project-config)

deploy: target/$(artifact-id)-$(version).jar
	clojure -T:build deploy $(project-config)
	@mv *.pom.asc target/

test:
	clojure -M:test

nuke: clean
	@echo "Nuking everything"
	@rm -rf .makecache
	@rm -rf .cpcache
	@rm -rf node_modules
	@rm -rf .shadow-cljs

clean:
	@echo "Cleaning target and resources"
	@rm -rf target
	@find resources -not -name \index.html -not -name \audio -mindepth 1 -prune -exec rm -rf {} \;

