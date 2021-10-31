with (import ./pkgs.nix).haskell-nix;
project {
  # 'cleanGit' cleans a source directory based on the files known by git
  src = haskellLib.cleanGit {
    name = "realworld-haskell";
    src = ./.;
  };
  # Specify the GHC version to use.
  compiler-nix-name = "ghc8107"; # Not required for `stack.yaml` based projects.

  index-state = "2021-10-07T00:00:00Z";

  # plan-sha256 = "RiN1455qlA4rp2yO6wJp9FCVzAOR20KzfmNMM3fPXTQ=";

  modules = [{
    packages.realworld-haskell.components.exes.realworld-haskell = {
      dontStrip = false;
      ghcOptions = [ "-O2" ];
    };
  }];
}
