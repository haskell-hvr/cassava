{ subDir ? ""
, ghc ? "ghc902"
, doCheck ? true
, configureFlags ? [ ]
, withHoogle ? true
, ...
}@args:
let
  haskellNixSrc = builtins.fetchTarball {
    url = "https://github.com/input-output-hk/haskell.nix/archive/b3df33abbcb736437cb06d5f53a10f6bb271bc51.tar.gz";
    sha256 = "11daql694vp0hxs9rkyb3cn50yjfy840bybpsmrcq208cdjm7m0q";
  };

  haskellNix = import haskellNixSrc { };

  pkgs = import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs;

  project = pkgs.haskell-nix.cabalProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      src = ./.;
      inherit subDir;
      name = "cassava";
    };
    name = "cassava";
    compiler-nix-name = ghc;
    index-state = "2022-09-03T00:00:00Z";
    modules = [
      { inherit doCheck configureFlags; }
    ];
  };

  shell =
    let hsPkgs = pkgs.haskell.packages.${ghc};
    in project.shellFor {

      inherit withHoogle;

      exactDeps = true;

      buildInputs = [
        hsPkgs.cabal-install
        hsPkgs.haskell-language-server
      ];

      LANG = "en_US.utf8";
      LC_ALL = "en_US.utf8";
      LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
      };

  compilers = pkgs.haskell.compiler;

in with builtins;
  if hasAttr ghc compilers
  then { inherit pkgs project shell; }
  else abort ("Unsupported GHC, available GHCs: " + concatStringsSep ", " (attrNames compilers))
