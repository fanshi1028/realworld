{ nixpkgsPin ? "unstable", ghcVersion ? "8107", checkMaterialization ? false
, materializedDir ? ./materialized, flags ? { } }:
with import ./nix/pkgs.nix { inherit nixpkgsPin; };
haskell-nix.project {
  # 'cleanGit' cleans a source directory based on the files known by git
  src = haskell-nix.haskellLib.cleanGit {
    name = "realworld-haskell";
    src = ./.;
  };

  # Specify the GHC version to use.
  compiler-nix-name = "ghc${ghcVersion}";

  index-state = "2022-02-01T00:00:00Z";

  modules = [
    {
      # https://github.com/composewell/streamly/issues/1132
      # https://github.com/NixOS/cabal2nix/issues/470
      # https://github.com/input-output-hk/haskell.nix/issues/1164
      packages.streamly.components.library.libs =
        lib.optionals (builtins.currentSystem == "x86_64-darwin")
        [ darwin.apple_sdk.frameworks.Cocoa ];
      packages.realworld-haskell = {
        inherit flags;
        components.exes.realworld-haskell = {
          dontStrip = false;
          ghcOptions = [ "-O2" ];
        };
      };
    }
    # https://github.com/input-output-hk/haskell.nix/issues/1111
    ({ pkgs, ... }: {
      packages.realworld-haskell.components.tests.realworld-haskell-test.build-tools =
        pkgs.lib.mkForce [ pkgs.postgresql_13 ];
    })
  ];

  # NOTE: no materialization as we change cabal file quite frequently at this stage of development
  # inherit checkMaterialization;

  # plan-sha256 = if materializedDir != null && ghcVersion == "8107"
  # && builtins.currentSystem == "x86_64-darwin" then
  #   "16bvlg18771clda8gydcm12syrf7igpgp2346n77lv20zc0l8av3"
  # else
  #   null;

  # materialized = if materializedDir != null && ghcVersion == "8107"
  # && builtins.currentSystem == "x86_64-darwin" then
  #   materializedDir + /haskell-nix
  # else
  #   null;
}
