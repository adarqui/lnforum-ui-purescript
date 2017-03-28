all:
	pulp browserify > static/dist/bundle.js
	make sync

sync:
	mkdir -p /Users/x/code/github/adarqui/lnotes-yesod/static/lnotes.dist/
	cp static/dist/bundle.js /Users/x/code/github/adarqui/lnotes-yesod/static/lnotes.dist/bundle.js

# https://www.npmjs.com/package/uglify-js
uglify:
#	ccjs static/dist/bundle.js --compilation_level=ADVANCED_OPTIMIZATIONS > static/dist/bundle.min.js
	uglifyjs --compress --mangle -- static/dist/bundle.js 2>/dev/null > static/dist/bundle.min.js

all-upload: all uglify upload-ugly

upload-ugly:
	rsync -av -e ssh ./static/dist/bundle.min.js lnotes:/projects/leuronet/lnotes-yesod/static/lnotes.dist/bundle.js

build:
	pulp -w build

tests:
	pulp -w test

bower:
	bower install

deps:
	npm install -g pulp
