{ pkgs ? import <nixpkgs> {} }:
with pkgs;
let ghcEnv = haskellPackages.ghcWithPackages (hp: with hp; [ megaparsec ]);
in
mkShell {
  buildInputs = [ ghcEnv ];
}
