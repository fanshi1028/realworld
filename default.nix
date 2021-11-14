with import ./nix/pkgs.nix;
haskell-nix.project {
  # 'cleanGit' cleans a source directory based on the files known by git
  src = haskell-nix.haskellLib.cleanGit {
    name = "realworld-haskell";
    src = ./.;
  };
  # Specify the GHC version to use.
  compiler-nix-name = "ghc8107"; # Not required for `stack.yaml` based projects.

  index-state = "2021-10-07T00:00:00Z";

  # plan-sha256 = "RiN1455qlA4rp2yO6wJp9FCVzAOR20KzfmNMM3fPXTQ=";

  modules = [{
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
  }];
}
