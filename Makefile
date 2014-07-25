.PHONY: clean-rebuild

clean-rebuild:
	cabal sandbox delete
	cabal sandbox init --sandbox .cabal-sandbox
	cabal sandbox add-source ../diff-parse
	cabal sandbox add-source ../github
	cabal install alex happy
	cabal install --dependencies-only
	cabal install yesod-bin

ngrok:
	ngrok -subdomain=reviewhub 3000
