{ nixpkgsPin ? "2111", ghcVersion ? if nixpkgsPin == "2111" then
  "8107"
else if nixpkgsPin == "unstable" then
  "921"
else
  null, checkMaterialization ? false }:
assert ghcVersion != null;
with import ./nix/pkgs.nix { inherit nixpkgsPin; };
haskell-nix.project {
  inherit checkMaterialization;

  # 'cleanGit' cleans a source directory based on the files known by git
  src = haskell-nix.haskellLib.cleanGit {
    name = "realworld-haskell";
    src = ./.;
  };
  # Specify the GHC version to use.
  compiler-nix-name = "ghc${ghcVersion}";

  index-state = "2021-12-31T00:00:00Z";

  modules = [
    {
      # https://github.com/composewell/streamly/issues/1132
      # https://github.com/NixOS/cabal2nix/issues/470
      # https://github.com/input-output-hk/haskell.nix/issues/1164
      packages.streamly.components.library.libs =
        lib.optionals (builtins.currentSystem == "x86_64-darwin")
        [ darwin.apple_sdk.frameworks.Cocoa ];
      packages.realworld-haskell.components.exes.realworld-haskell = {
        dontStrip = false;
        ghcOptions = [ "-O2" ];
      };
    }
    # https://github.com/input-output-hk/haskell.nix/issues/1111
    ({ pkgs, ... }: {
      packages.realworld-haskell.components.tests.realworld-haskell-test.build-tools =
        pkgs.lib.mkForce [ pkgs.postgresql_13 ];
    })
  ];

  plan-sha256 = if ghcVersion == "8107" then
    "07m98qfk9y6smlgyg75dva2mq8037gp5m8ld46jycwagnhns3kai"
  else
    null;

  materialized =
    if ghcVersion == "8107" then ./materialized/haskell-nix else null;
}
