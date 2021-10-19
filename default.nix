let
  # Read in the Niv sources
  sources = import ./nix/sources.nix { };
  # If ./nix/sources.nix file is not found run:
  #   niv init
  #   niv add input-output-hk/haskell.nix -n haskellNix

  # Fetch the haskell.nix commit we have pinned with Niv
  haskellNix = import sources.haskellNix { };
  # If haskellNix is not found run:
  #   niv add input-output-hk/haskell.nix -n haskellNix

  # Import nixpkgs and pass the haskell.nix provided nixpkgsArgs
  pkgs = import
    # haskell.nix provides access to the nixpkgs pins which are used by our CI,
    # hence you will be more likely to get cache hits when using these.
    # But you can also just use your own, e.g. '<nixpkgs>'.
    haskellNix.sources.nixpkgs-2105
    # These arguments passed to nixpkgs, include some patches and also
    # the haskell.nix functionality itself as an overlay.
    haskellNix.nixpkgsArgs;
in pkgs.haskell-nix.project {
  # 'cleanGit' cleans a source directory based on the files known by git
  src = pkgs.haskell-nix.haskellLib.cleanGit {
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
    };
  }];
}
