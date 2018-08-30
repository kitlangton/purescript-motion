setup: install-deps build

build:
	psc-package build

install-deps:
	yarn install

serve:
	parcel serve index.html

deploy:
	pulp build && parcel build && surge dist -d "purescript-motion.surge.sh"
