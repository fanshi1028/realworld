# shell.nix
{ checkMaterialization ? false }:
let
  project = import ./default.nix;
  index-state = "2021-10-07T00:00:00Z";
  materializedDir = ./materialized;
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
    cabal = {
      inherit index-state checkMaterialization;
      version = "3.6.0.0";
      plan-sha256 = "0n46d3pa5jhbv3z4hjywn83lfxpdcbcc3dmipdywdjykmk9ig44r";
      materialized = materializedDir + /cabal;
    };
    hlint = {
      inherit index-state checkMaterialization;
      version = "3.3.4";
      plan-sha256 = "0sycg9swgh7isv8wxr91l6hxvrbaynwznaspd0cwjghaxh75s68l";
      materialized = materializedDir + /hlint;
    };
    haskell-language-server = {
      inherit index-state checkMaterialization;
      version = "1.4.0.0";
      plan-sha256 = "03ksqzhnd1qn2rizhdr0vf34d8y6r49pb8mn9qmylzqfsmi5cb41";
      materialized = materializedDir + /haskell-language-server;
    };
    ormolu = {
      inherit index-state checkMaterialization;
      version = "0.3.0.1";
      plan-sha256 = "17fgqh434ndvb55n3bwmmyh2qgiyam4rxl4wwdks7r9fsxy8jq2n";
      materialized = materializedDir + /ormolu;
    };
    ghcid = {
      inherit index-state checkMaterialization;
      version = "0.8.7";
      plan-sha256 = "0icxgap4qg8v8xbnsxldanj5brlhs0sq6c11xmgwhkfa0b7n72i2";
      materialized = materializedDir + /ghcid;
    };
    # use cabal-docspec instead of doctest(which I failed to set it up right), yet it seesm that cabal-docspec is not on hackage yet
    # https://github.com/phadej/cabal-extras/tree/master/cabal-docspec
    # doctest = "latest";
    cabal-fmt = {
      inherit index-state checkMaterialization;
      version = "0.1.5.1";
      plan-sha256 = "00w8vj9s9ah0cvqxkrnfr3xhxmgra0bwlv1sxp4inn2s88ixnn5q";
      materialized = materializedDir + /cabal-fmt;
    };
    stan = {
      inherit index-state checkMaterialization;
      version = "0.0.1.0";
      plan-sha256 = "18iw1gm915rp8j9h9rgiks80yyww0h1zg1niq0zyyqgaz9q54nv5";
      materialized = materializedDir + /stan;
    };
    # hoogle = {
    #   inherit index-state checkMaterialization;
    #   version = "5.0.17.15";
    #   plan-sha256 = "Z+9k15uSfml5rO/Badx0Ud+TSI/Wu92gPPTWqHBiaM4=";
    #   materialized = materializedDir + /hoogle;
    # };
  };
  # See overlays/tools.nix for more details

  # Some you may need to get some other way.
  buildInputs = [ ];

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
