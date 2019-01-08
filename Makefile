.PHONY: build
build:
	dune build

.PHONY: test
test:
	dune runtest

.PHONY: clean
clean:
	dune clean

_opam:
	opam switch create . --empty
	opam install -y ocaml-base-compiler.4.06.1 merlin utop ocp-indent

install-dependencies: _opam
	opam pin git@github.com:AestheticIntegration/ocyaml.git
	opam install . --deps-only --with-test

.PHONY: doc
doc:
	dune build @doc

DOCS_WORKTREE_PATH=../ocaml-decoders-doc

.PHONY: publish-doc
publish-doc: doc
	[ -d "$(DOCS_WORKTREE_PATH)" ] || git worktree add "$(DOCS_WORKTREE_PATH)" gh-pages
	cd "$(DOCS_WORKTREE_PATH)" && git pull --ff-only
	cd "$(DOCS_WORKTREE_PATH)" && git rm -r .
	cp -r _build/default/_doc/_html/* "$(DOCS_WORKTREE_PATH)"
	COMMIT_SHA=$$(git rev-parse HEAD) && cd "$(DOCS_WORKTREE_PATH)" && git add . && git commit -m "Update docs from revision $${COMMIT_SHA}"
	cd "$(DOCS_WORKTREE_PATH)" && git push origin gh-pages
