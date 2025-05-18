{ pkgs ? import <nixpkgs> {} }:
with pkgs;
let ghcEnv = haskellPackages.ghcWithPackages (hp: with hp; [ ]);
in
mkShell {
  buildInputs = [ ghcEnv ];
}
