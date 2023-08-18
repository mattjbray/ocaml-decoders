.PHONY: all
all: build test build-bs test-bs

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
	opam install -y ocaml-base-compiler.4.12.0 utop ocaml-lsp-server

install-dependencies: _opam
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

watch-build-bs:
	npm run build-watch

watch-test-bs:
	npm run test-watch

clean-bs:
	npm run clean

js/melange-decoders.opam:
	cp melange-decoders.opam js/melange-decoders.opam

melange-decoders-test: js/melange-decoders.opam
	cd js && dune build __tests__
	npx jest js/_build

melange-decoders.install: js/melange-decoders.opam
	cd js && dune build -p melange-decoders
	perl -i -pe s#_build#js/_build# js/melange-decoders.install
	mv js/melange-decoders.install .
