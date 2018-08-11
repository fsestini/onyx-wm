default: hpack
	cabal new-build all

core: hpack
	cabal new-build onyx-core

wm: hpack
	cabal new-build onyx-wm

run:
	cabal new-run onyx-wm-exe

hpack:
	(cd onyx-core ; hpack)
	(cd onyx-wm ; hpack)

clean:
	rm -rf dist
	rm -rf dist-newstyle
