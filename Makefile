build:
	mkdir -p dist
	ghc --make -o dist/project-euler-haskell src/*.hs -odir compilation-files -hidir compilation-files

run:
	./dist/project-euler-haskell

dev: build run
