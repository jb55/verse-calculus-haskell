{ pkgs ? import <nixpkgs> {} }:
with pkgs;
let ghcEnv = haskellPackages.ghcWithPackages (hp: with hp; [ megaparsec QuickCheck ]);
in
mkShell {
  buildInputs = [ ghcEnv ];
}
