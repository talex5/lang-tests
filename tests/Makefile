all: ocaml haskell

ocaml:
	make -C ../ocaml
	./test_launcher.py ../ocaml/runsels.native

haskell:
	make -C ../haskell
	./test_launcher.py ../haskell/runsels
