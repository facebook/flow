VERSION ?= $(shell node -p 'require("./package.json").version')
FLOW_VERSION := $(patsubst v%,%,$(VERSION))
FLOW_BINS = \
	flow-linux64-v$(FLOW_VERSION)/flow \
	flow-linux-arm64-v$(FLOW_VERSION)/flow \
	flow-osx-arm64-v$(FLOW_VERSION)/flow \
	flow-win64-v$(FLOW_VERSION)/flow.exe

.PHONY: all
all: clean build test

.PHONY: clean
clean:
	rm -f .npmrc
	rm -rf flow-*-v* SHASUM256.txt SHASUM256.txt.sign signing.key signing.pem

.PHONY: bump
bump:
	sed -i.bak 's/"version": ".*"/"version": "$(FLOW_VERSION)"/' package.json
	rm package.json.bak

.PHONY: test
test: $(FLOW_BINS)
	shasum -c SHASUM256.txt
	node test.js

.PHONY: build
build: clean bump SHASUM256.txt

.PHONY: pack
pack: build SHASUM256.txt.sign signing.pem
	npm pack

.PHONY: publish
publish: build test SHASUM256.txt.sign signing.pem
ifneq ("$(NPM_TOKEN)", "")
	@echo "//registry.npmjs.org/:_authToken=${NPM_TOKEN}" > .npmrc
else
	@echo "NPM_TOKEN not set. Either set a token or run 'npm adduser' to log in"
endif
	npm publish --tag $(if $(findstring -,$(FLOW_VERSION)),next,latest)

SHASUM256.txt: $(FLOW_BINS)
	shasum -a 256 $^ > $@

get-flow = \
	curl -O -L https://github.com/facebook/flow/releases/download/v$(*F)/$(@D).zip; \
	unzip $(@D).zip flow/$(@F); \
	mv flow $(@D); \
	rm $(@D).zip; \
	touch $@

flow-linux64-v%/flow:
	$(get-flow)

flow-linux-arm64-v%/flow:
	$(get-flow)

flow-osx-arm64-v%/flow:
	$(get-flow)

flow-win64-v%/flow.exe:
	$(get-flow)


#################################
# Code signing
#
# The binaries' hashes are stored in SHASUM256.txt. To ensure integrity of
# SHASUM256.txt, we also provide SHASUM256.txt.sign, which is signed with our
# (self-signed) SSL certificate, signing.pem.
#
# The base64 encoding of the private key must be passed as an environment
# variable named "FLOW_BIN_PRIVATE_KEY_BASE64".
#
#################################

# base64 decodes the private key and writes it into a file named signing.key
signing.key:
ifeq ("$(FLOW_BIN_PRIVATE_KEY_BASE64)", "")
	@echo "FLOW_BIN_PRIVATE_KEY_BASE64 variable not set."
	exit 1
else
	@echo '$(FLOW_BIN_PRIVATE_KEY_BASE64)' | base64 -d > $@
endif

# generates the public key from signing.key
#
# this cert also needs to be included in flow-for-vscode or anywhere else that
# wants to verify our release binaries offline.
signing.pem: signing.key
	openssl rsa -in "$<" -pubout -out "$@"

%.sign: % signing.key
	openssl dgst -sign signing.key -sha256 -out "$@.bin" -binary "$<"
	openssl base64 -in "$@.bin" -out "$@"
	rm -f "$@.bin"

verify: SHASUM256.txt signing.pem SHASUM256.txt.sign
	openssl base64 -d -in SHASUM256.txt.sign -out SHASUM256.txt.sign.bin
	openssl dgst -verify signing.pem -sha256 -signature SHASUM256.txt.sign.bin SHASUM256.txt
	rm -f "SHASUM256.txt.sign.bin"
