{ nixpkgsPin ? "unstable", ghcVersion ? "8107", checkMaterialization ? false
, materializedDir ? ./materialized, exeFlag ? null
  # NOTE: https://www.parsonsmatt.org/2019/11/27/keeping_compilation_fast.html
  #  ghcOptions:  [ "-j" "-O2" "+RTS -A128m -n2m -RTS" ]
, threads ? "", optimize ? 2, RTS ? "-N -A128m -n2m", ghcOptions ? [ ] }:
with import ./nix/pkgs.nix { inherit nixpkgsPin; };
haskell-nix.project {
  # 'cleanGit' cleans a source directory based on the files known by git
  src = haskell-nix.haskellLib.cleanGit {
    name = "realworld-haskell";
    src = ./.;
  };

  cabalProjectFileName = lib.mkForce
    ("cabal.project" + lib.optionalString (exeFlag != null) ".${exeFlag}");

  # Specify the GHC version to use.
  compiler-nix-name = "ghc${ghcVersion}";

  index-state = "2022-02-10T00:00:00Z";

  modules = [{
    # https://github.com/composewell/streamly/issues/1132
    # https://github.com/NixOS/cabal2nix/issues/470
    # https://github.com/input-output-hk/haskell.nix/issues/1164
    # TEMP FIXME
    packages.streamly.components.library.libs =
      lib.optionals (builtins.currentSystem == "x86_64-darwin")
      [ darwin.apple_sdk.frameworks.Cocoa ];

    packages.realworld-haskell = {
      # NOTE: https://github.com/input-output-hk/haskell.nix/issues/1165
      # flags = lib.genAttrs cabalFlags (flag: lib.mkOverride 10 true);
      ghcOptions = [
        "-j${builtins.toString threads}"
        "-O${builtins.toString optimize}"
        "+RTS ${RTS} -RTS"
      ] ++ ghcOptions;
      components.exes =
        lib.genAttrs [ "frontend" "backend" ] (_: { dontStrip = false; });
    };
  }] ++ lib.optionals (exeFlag == "backend-rel8") [
    # https://github.com/input-output-hk/haskell.nix/issues/1111
    ({ pkgs, ... }: {
      packages.realworld-haskell.components.tests.realworld-haskell-test.libs =
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
