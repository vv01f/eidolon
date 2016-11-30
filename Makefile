all:	hsbuild pursbuild

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

pursbuild:	pursdeps
	pulp browserify -m Login -O -t ./static/js/purs_login.js
	pulp browserify -m Activate -O -t ./static/js/purs_activate.js
