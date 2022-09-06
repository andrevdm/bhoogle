package = bhoogle
exe = bhoogle

run:
	cabal run $(exe)

build:
	cabal build $(package) --ghc-options "-j6 +RTS -A128m -n2m -qg -RTS"

build-fast:
	cabal build $(package) --disable-optimisation --ghc-options "-O0 -j6 +RTS -A128m -n2m -qg -RTS"

ghcid:
	ghcid --lint -c "cabal repl --repl-options='-ignore-dot-ghci' --repl-options='-fobject-code' --repl-options='-fno-warn-unused-do-bind' --repl-options='-j6' "

