all:
	 ocamlbuild -cflags -g -use-ocamlfind -pkg xmlm,unix,str runsels.native

doc:
	ocamlbuild -use-ocamlfind -pkg xmlm,unix,str 0install.docdir/index.html

run: all
	OCAMLRUNPARAM=b ./runsels.native ../selections2.xml
