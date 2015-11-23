cabal2nix --shell . > shell.nix
nix-shell -I . --command 'cabal clean && cabal configure'

