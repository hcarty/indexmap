all:
	ocamlbuild -use-ocamlfind -package batteries indexMap.cma indexMap.cmxa indexMap.cmxs batIndexMap.cma batIndexMap.cmxa batIndexMap.cmxs

clean:
	ocamlbuild -clean

install:
	ocamlfind install indexmap META indexMap.mli batIndexMap.mli _build/indexMap.cm[ia] _build/indexMap.cmx[sa] _build/batIndexMap.cm[ia] _build/batIndexMap.cmx[sa]

uninstall:
	ocamlfind remove indexmap

bench:
	ocamlbuild -use-ocamlfind -package unix,bigarray,bench t/bench_test.native
