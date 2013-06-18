all:
	 ocamlbuild -cflags "-g,-w A-4" -use-ocamlfind -pkg yojson,xmlm,unix,str runsels.native
	 ocamlbuild -cflags "-g,-w A" -use-ocamlfind -pkg yojson,unix runenv.native

byte:
	 ocamlbuild -cflags -g -use-ocamlfind -pkg xmlm,unix,str runsels.d.byte

doc:
	ocamlbuild -use-ocamlfind -pkg yojson,xmlm,unix,str 0install.docdir/index.html

run: all
	OCAMLRUNPARAM=b ./runsels.native ../selections2.xml
