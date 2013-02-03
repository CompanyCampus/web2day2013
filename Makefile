all: build

build: site
	./site build

site: site.hs LinkedCompilers.hs RouteFactories.hs Utils.hs
	ghc --make site.hs
	./site clean

preview: site
	./site preview

clean: site
	./site clean

mrproper: clean
	rm site
	rm site.hi
	rm site.o
