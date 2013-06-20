all: runsels

runsels: *.hs
	ghc -Wall runsels.hs
	ghc -Wall Runenv.hs

doc:
	haddock -o html -h Apps.hs Basedir.hs Bindings.hs Command.hs Config.hs Run.hs runsels.hs Selections.hs Support.hs
