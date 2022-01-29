# shell.nix
{ nixpkgsPin ? "unstable", ghcVersion ? "8107", checkMaterialization ? false
, materializedDir ? ./materialized }:
let
  project = import ./default.nix {
    inherit nixpkgsPin ghcVersion checkMaterialization;
  };
  index-state = project.index-state;

  tool-common = { inherit index-state checkMaterialization; };
  ony-ghc8107-materialize-config = plan-sha256: subpath:
    if materializedDir != null && ghcVersion == "8107" then {
      inherit plan-sha256;
      materialized = materializedDir + subpath;
    } else
      { };
  tool-config = { version, plan-sha256, materialized, ... }@args:
    tool-common // args // {
      inherit version;
    } // (ony-ghc8107-materialize-config plan-sha256 materialized);
in project.shellFor {
  # ALL of these arguments are optional.

  # List of packages from the project you want to work on in
  # the shell (default is all the projects local packages).
  packages = ps: with ps; [ realworld-haskell ];

  # Builds a Hoogle documentation index of all dependencies,
  # and provides a "hoogle" command to search the index.
  withHoogle = true;

  # Some common tools can be added with the `tools` argument
  tools = {
    cabal = tool-config {
      version = "3.6.2.0";
      plan-sha256 = "03i9rdvnpkr96x3ng5zfvfd9h49qsyzmxlckh2i1yr4xn991yid3";
      materialized = /cabal;
    };
    hlint = tool-config {
      version = "3.3.6";
      plan-sha256 = "17km3wxl79sl6vcgjl3yadqm41lb503hd8vsr9rc0z95yg20n91j";
      materialized = /hlint;
    };
    haskell-language-server = tool-config {
      version = "1.5.1.0";
      plan-sha256 = "0vhq2gvbwssgbh54ciwg2jcjsqsbrc9ml54yqnvv758ykga6xrsk";
      materialized = /haskell-language-server;
    };
    # error: builder for '/nix/store/9w46v4709ddiycqg6zdrssfwsjlz64nq-ormolu-lib-ormolu-0.4.0.0.drv' failed with exit code 1
    ormolu = tool-config {
      version = "0.4.0.0";
      plan-sha256 = "1g1g88bi46lx7kf2zc7lq7bgcqvcs5h7d53v5zclhgihfww1w5hl";
      materialized = /ormolu;
      # TEMP FIXME NOTE: https://github.com/input-output-hk/haskell.nix/issues/1337
      modules = [
        ({ lib, ... }: {
          options.nonReinstallablePkgs =
            lib.mkOption { apply = lib.remove "Cabal"; };
        })
      ];
    };
    ghcid = tool-config {
      version = "0.8.7";
      plan-sha256 = "0z35zpx7x0qncqiddnq4acdq3cgqlalwc2kb28dv8lk1rvn377ri";
      materialized = /ghcid;
    };
    # use cabal-docspec instead of doctest(which I failed to set it up right), yet it seesm that cabal-docspec is not on hackage yet
    # https://github.com/phadej/cabal-extras/tree/master/cabal-docspec
    # doctest = "latest";
    cabal-fmt = tool-config {
      version = "0.1.5.1";
      plan-sha256 = "1wbds3wmnfzl9g562271bvd8z1whmiv2cn10m3qxbfpbdj3ri25h";
      materialized = /cabal-fmt;
    };
    stan = tool-config {
      version = "0.0.1.0";
      plan-sha256 = "1b5ckkajsf87jczavx18glwfa06zcvi7w1dp45xbpiyjqf7wmpi2";
      materialized = /stan;
    };
    hoogle = tool-config {
      version = "5.0.18.3";
      plan-sha256 = "0knhl9icjpmbqz18vw4pxs6n5m6m32b1jyss6cmlz86s6df7pwik";
      materialized = /hoogle;
    };
  };
  # See overlays/tools.nix for more details

  # Some you may need to get some other way.
  buildInputs = with import ./nix/pkgs.nix { inherit nixpkgsPin; }; [
    postgresql_13
    sqls
  ];

  # Sellect cross compilers to include.
  crossPlatforms = ps:
    with ps;
    [
      # ghcjs # Adds support for `js-unknown-ghcjs-cabal build` in the shell
      # mingwW64 # Adds support for `x86_64-W64-mingw32-cabal build` in the shell
    ];

  # Prevents cabal from choosing alternate plans, so that
  # *all* dependencies are provided by Nix.
  exactDeps = true;
}
