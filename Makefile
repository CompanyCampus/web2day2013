all: build

build: site
	./site build

site: site.hs
	ghc --make site.hs

preview: site
	./site preview

clean: site
	./site clean
	rm site
