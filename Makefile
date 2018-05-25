.PHONY: build
build:
	jbuilder build

.PHONY: test
test:
	jbuilder runtest

.PHONY: clean
clean:
	jbuilder clean

.PHONY: doc
doc:
	jbuilder build @doc

_opam:
	opam switch create . --empty
	opam install -y ocaml-base-compiler.4.06.1 merlin utop ocp-indent
