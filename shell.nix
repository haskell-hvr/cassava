{ doCheck ? true
, ghc ? "ghc902"
, withHoogle ? true
, configureFlags ? [ ]
}:
let
  result = import ./default.nix {
    inherit configureFlags doCheck ghc;
  };

  inherit (result) pkgs project;
  hsPkgs = pkgs.haskell.packages.${ghc};

in project.shellFor {

  inherit withHoogle;

  exactDeps = true;

  # We have to add 'criterion' explicitly as it depends on 'cassava' itself,
  # so it will be dropped from included dependencies and benchmarks build fails.
  # https://github.com/input-output-hk/haskell.nix/blob/b3df33abbcb736437cb06d5f53a10f6bb271bc51/builder/shell-for.nix#L31-L49)
  additional = ps: [ps.criterion.components.library];

  buildInputs = [
    hsPkgs.cabal-install
    hsPkgs.haskell-language-server
  ];

  LANG = "en_US.utf8";
  LC_ALL = "en_US.utf8";
  LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
}
