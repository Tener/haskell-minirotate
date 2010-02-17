
all: build

clean:
	cabal clean
	find -name "*~" -print -delete

build:
	cabal configure && cabal build