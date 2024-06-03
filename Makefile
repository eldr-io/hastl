help: ## Print documentation
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

ghcid-devel: ## Run the server in fast development mode. See DevelMain for details.
	ghcid \
	    --command "cabal repl hastl" \
	    --test DevelMain.update \
	    --warnings \
	    --restart ./hastl.cabal

imports: ## Format all the imports that have changed since the master branch.
	./stylish-haskell.sh

.PHONY: tailwind-watch
tailwind-watch:
	./tailwindcss -i ./static/css/custom.css -o ./static/css/style.css --watch

.PHONY: tailwind-build
tailwind-build:
	./tailwindcss -i ./static/css/custom.css -o ./static/css/style.min.css --minify

.PHONY: build
build: tailwind-build
	cabal build

.PHONY: run
run: tailwind-build
	cabal run

.PHONY: ghcid-devel help imports
