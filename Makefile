.PHONY: help
help: ## print make targets 
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: ghcid-devel
ghcid-devel: tailwind-build ## Run the server in fast development mode. See lib/DevelMain.hs for details.
	ghcid \
	    --command "cabal repl hastl" \
	    --test DevelMain.update \
	    --warnings \
	    --restart ./hastl.cabal

.PHONY: tailwind-watch
tailwind-watch: ## compile tailwindcss and watch for changes
	./tailwindcss -i ./static/css/custom.css -o ./static/css/style.css --watch

.PHONY: tailwind-build
tailwind-build: ## one-time compile tailwindcss styles
	./tailwindcss -i ./static/css/custom.css -o ./static/css/style.css

.PHONY: build
build: tailwind-build ## compiles tailwindcss styles and builds the project with cabal
	cabal build

.PHONY: run
run: tailwind-build ## compiles tailwindcss styles and runs the project with cabal
	cabal run

.PHONY: test
test: ## runs the unit test project
	cabal test hastl-test --enable-tests --test-show-details=direct

.PHONY: test-integration
test-integration: ## runs the integration test project
	cabal test hastl-test-integration --enable-tests --test-show-details=direct
