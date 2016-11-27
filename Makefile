clean:
	cabal clean
	rm -r bower_components
	rm -r node_modules
	rm ./static/js/purs.js

hsdeps:
	cabal install --only-dependencies

hsbuild:	hsdeps
	cabal build

pursdeps:	cryptojs
	bower install

cryptojs:
	npm install crypto-js
	npm install react
	npm install react-dom

pursbuild:	purdeps
	pulp browserify -O > ./static/js/purs.js

all:	hsbuild pursbuild
