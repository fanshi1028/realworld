# shell.nix
{ nixpkgsPin ? "unstable", ghcVersion ? "8107", checkMaterialization ? false
, materializedDir ? ./materialized }:
let
  project = import ./default.nix {
    inherit nixpkgsPin ghcVersion materializedDir checkMaterialization;
  };
  index-state = project.index-state;

  tool-common = { inherit index-state checkMaterialization; };
  ony-ghc8107-materialize-config = subpath:
    if materializedDir != null && ghcVersion == "8107" then {
      materialized = materializedDir + subpath;
    } else {
      plan-sha256 = null;
      materialized = null;
    };
  tool-config = { materialized, ... }@args:
    tool-common // args // (ony-ghc8107-materialize-config materialized);
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
      plan-sha256 = "0kpz9yvhyn7xjsh0cryp5rs1wypbcg9s67czyqly2p61a1g2qz8p";
      materialized = /cabal;
    };
    hlint = tool-config {
      version = "3.3.6";
      plan-sha256 = "1h81llf0a37jjx2im84dbj4dbfwn22d0zs5mw51cicx0fq8nch7a";
      materialized = /hlint;
    };
    haskell-language-server = tool-config {
      version = "1.6.1.0";
      plan-sha256 = "13nhxjyrlrs79rni969d7sslrqwx2nlxg92qc187m0aj95sn3cwg";
      materialized = /haskell-language-server;
    };
    # error: builder for '/nix/store/9w46v4709ddiycqg6zdrssfwsjlz64nq-ormolu-lib-ormolu-0.4.0.0.drv' failed with exit code 1
    ormolu = tool-config {
      version = "0.4.0.0";
      plan-sha256 = "1a9q0djrkr97s6hk2byjv70dis18gfgphsb86k32dsavgbxzzn29";
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
      plan-sha256 = "1qj316s6qnpdgw33sfl4k5266whh4sx60z2svqa5r9b4pkj8a3hi";
      materialized = /ghcid;
    };
    # use cabal-docspec instead of doctest(which I failed to set it up right), yet it seesm that cabal-docspec is not on hackage yet
    # https://github.com/phadej/cabal-extras/tree/master/cabal-docspec
    # doctest = "latest";
    cabal-fmt = tool-config {
      version = "0.1.5.1";
      plan-sha256 = "02pkllcs1l82gcsilxpykz3mh5d3vdfhma1a1543h9faqsgwvbc6";
      materialized = /cabal-fmt;
    };
    stan = tool-config {
      version = "0.0.1.0";
      plan-sha256 = "07xl7aqxvdbpfjlsbx1qzczm8rs4w4zham4pn4clj5717jp8qbms";
      materialized = /stan;
    };
    hoogle = tool-config {
      version = "5.0.18.3";
      plan-sha256 = "1q1r349mpcbdzis0lncwwfvml74qpp5hlyppc3pgzzbqnwwgq6nh";
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
