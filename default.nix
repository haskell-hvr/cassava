{ ghc
, doCheck ? true
, configureFlags ? [ ]
}:
let
  haskellNixSrc = builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/b3df33abbcb736437cb06d5f53a10f6bb271bc51.tar.gz";

  haskellNix = import haskellNixSrc { };

  pkgs = import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs;

in {
  inherit pkgs;

  project = pkgs.haskell-nix.cabalProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "cassava";
      src = ./.;
    };
    name = "cassava";
    compiler-nix-name = ghc;
    index-state = "2022-09-03T00:00:00Z";
    modules = [
      { inherit doCheck configureFlags; }
    ];
  };

}
