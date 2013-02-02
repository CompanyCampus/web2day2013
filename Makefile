all: build

build: site
	./site build

site: site.hs
	ghc --make site.hs
	./site clean

preview: site
	./site preview

clean: site
	./site clean
	rm site.hi
	rm site.o
	rm site
