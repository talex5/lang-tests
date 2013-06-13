all:
	 ocamlbuild -cflags -g -use-ocamlfind -pkg xmlm,unix,str runsels.native

run: all
	OCAMLRUNPARAM=b ./runsels.native ../selections3.xml
