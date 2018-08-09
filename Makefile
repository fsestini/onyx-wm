default: hpack
	cabal new-build all

run:
	cabal new-run onyx-wm-exe

hpack:
	(cd onyx-core ; hpack)
	(cd onyx-wm ; hpack)

clean:
	rm -rf dist
	rm -rf dist-newstyle
