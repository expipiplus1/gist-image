{ pkgs ? (import ./pkgs.nix) {} }:

with (import ./haskell-packages.nix) {inherit pkgs;};
with haskellPackages; 
with pkgs;

haskellPackageGen {
  extraEnvPackages = [ hdevtools ghc-mod stylish-haskell hlint hindent ];
} ./.
