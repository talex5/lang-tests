all: test_ocaml test_haskell

ocaml:
	make -C ../ocaml

test_ocaml: ocaml
	./test_launcher.py ../ocaml/runsels.native

haskell:
	make -C ../haskell

test_haskell: haskell
	./test_launcher.py ../haskell/runsels
