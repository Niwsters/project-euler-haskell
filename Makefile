build:
	ghc --make -o dist/project-euler-haskell src/*.hs

run:
	./dist/project-euler-haskell

dev: build run
