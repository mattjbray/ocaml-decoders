.PHONY: all
all: build test build-bs test-bs test-mel

.PHONY: clean-all
clean-all: clean clean-bs

.PHONY: build
build:
	dune build

.PHONY: test
test:
	dune runtest

.PHONY: watch-check
watch-check:
	dune build @check --watch

.PHONY: watch-test
watch-test:
	dune build @runtest --watch

.PHONY: doc
doc:
	dune build @doc

.PHONY: format
format:
	dune build @fmt --auto-promote

.PHONY: clean
clean:
	dune clean

_opam:
	opam switch create . --empty

install-dependencies: _opam
	opam switch set-invariant ocaml-base-compiler.5.1.1
	opam install . --deps-only --with-test

DOCS_WORKTREE_PATH=../ocaml-decoders-doc

.PHONY: publish-doc
publish-doc: doc
	[ -d "$(DOCS_WORKTREE_PATH)" ] || git worktree add "$(DOCS_WORKTREE_PATH)" gh-pages
	cd "$(DOCS_WORKTREE_PATH)" && git pull --ff-only
	cd "$(DOCS_WORKTREE_PATH)" && git rm -r .
	cp -r _build/default/_doc/_html/* "$(DOCS_WORKTREE_PATH)"
	COMMIT_SHA=$$(git rev-parse HEAD) && cd "$(DOCS_WORKTREE_PATH)" && git add . && git commit -m "Update docs from revision $${COMMIT_SHA}"
	cd "$(DOCS_WORKTREE_PATH)" && git push origin gh-pages



install-bs-dependencies:
	npm i

build-bs:
	npm run build

test-bs:
	npm test

clean-bs:
	npm run clean

build-mel:
	dune build @melange

test-mel:
	npx jest _build/default/out/
