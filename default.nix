let
  pkgsSrc = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/3364b5b117f65fe1ce65a3cdd5612a078a3b31e3.tar.gz";
    sha256 = "sha256-Bs6/5LpvYp379qVqGt9mXxxx9GSE789k3oFc+OAL07M=";
  };
in
{
  compiler ? "ghc944",
  pkgs ? import pkgsSrc {
    config = {
      allowBroken = false;
      allowUnfree = true;
    };

    overlays = [
      (self: super: {
        libbase64 = self.callPackage ./libbase64.nix {};
      })
    ];
  },
  returnShellEnv ? false,
}:

let
  nix-gitignore = import (pkgs.fetchFromGitHub {
    owner = "hercules-ci";
    repo = "gitignore";
    rev = "9e80c4d83026fa6548bc53b1a6fab8549a6991f6";
    sha256 = "04n9chlpbifgc5pa3zx6ff3rji9am6msrbn1z3x1iinjz2xjfp4p";
  }) {};

  lib = pkgs.lib;

  hsPkgs = pkgs.haskell.packages.${compiler};
  hsLib = pkgs.haskell.lib;

  # We need this because cabal2nix is a goofy goober
  removeHsBase64 = lib.lists.remove hsPkgs.base64;

  libbase64-bindings = (hsPkgs.developPackage {
    name = "libbase64-bindings";
    root = nix-gitignore.gitignoreSource ./.;

    modifier = drv: hsLib.overrideCabal drv (old: {
      librarySystemDepends = removeHsBase64 (old.librarySystemDepends or []) ++ [pkgs.libbase64];

      configureFlags = (old.configureFlags or []) ++ [
        "--extra-lib-dirs=${pkgs.libbase64}/lib"
        "--extra-include-dirs=${pkgs.libbase64}/include"
      ];
    });

    inherit returnShellEnv;
  }).overrideAttrs (old: {
    buildInputs = removeHsBase64 (old.buildInputs or []);
  });
in
{
  inherit pkgs;
  inherit (pkgs) libbase64;
  inherit libbase64-bindings;
}
