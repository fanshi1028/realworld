{
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  description = "realworld haskell";
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    # NOTE: https://github.com/NixOS/hydra/blob/cf9f38e43fd81f9298e3f2ff50c8a6ee0acc3af0/flake.nix#L4
    # NOTE: https://github.com/input-output-hk/haskell.nix/issues/1407#issuecomment-1073307982
    # NOTE: https://github.com/purefn/warp-hello/blob/980265e819c662450fe9399697b736d89598e3df/flake.nix#L10
    haskellNix.inputs.nixpkgs.follows = "nixpkgs";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        supported-compilers = [
          "ghc8107"
          "ghc922"
          # TEMP FIXME https://github.com/haskell/haskell-language-server/issues/2985
          "ghc923"
        ];
        backend-exes = [ "in-mem" "rel8" ];
        frontend-exes = [ "js" "vty" "warp" ];
        exes = backend-exes ++ frontend-exes;
        name = "realworld-haskell";
        overlays = [
          haskellNix.overlay
          (final: prev: {
            # This overlay adds our project to pkgs
            realworld-haskell-helper = compiler-nix-name: exe:
              assert pkgs.lib.assertOneOf "exe" exe exes
                && pkgs.lib.assertOneOf "compiler-nix-name" compiler-nix-name
                supported-compilers;
              final.haskell-nix.project' {
                src = prev.haskell-nix.haskellLib.cleanGit {
                  inherit name;
                  src = ./.;
                };
                inherit compiler-nix-name;

                cabalProjectFileName = pkgs.lib.mkForce "cabal.project.${
                    if builtins.elem exe backend-exes then
                      "backend"
                    else
                      "frontend"
                  }-${exe}";
                # This is used by `nix develop .`
                shell = {
                  # to open a shell for use with
                  # `cabal`, `hlint` and `haskell-language-server`
                  tools = {
                    cabal = { };
                    hlint = { };
                    # TEMP FIXME https://github.com/haskell/haskell-language-server/issues/2179
                    # TEMP FIXME https://github.com/input-output-hk/haskell.nix/issues/1272
                    haskell-language-server = pkgs.lib.optionalAttrs
                      (builtins.elem compiler-nix-name [ "ghc922" "ghc923" ]) {
                        version = "latest";
                        cabalProject = ''
                          packages: .
                          package haskell-language-server
                          flags: -haddockComments
                        '';
                      };
                  };
                  # Non-Haskell shell tools go here
                  buildInputs = with pkgs; [ nixpkgs-fmt ];
                  # This adds `js-unknown-ghcjs-cabal` to the shell.
                  # FIXME
                  # crossPlatforms = p:
                  #   # [ p.ghcjs ];
                  #   pkgs.lib.optionals (exe == "frontend-js") [ p.ghcjs ];
                };
                modules = [{
                  # https://github.com/composewell/streamly/issues/1132
                  # https://github.com/NixOS/cabal2nix/issues/470
                  # https://github.com/input-output-hk/haskell.nix/issues/1164
                  # TEMP FIXME
                  packages.streamly.components.library.libs =
                    nixpkgs.lib.optionals (system == "x86_64-darwin")
                    [ pkgs.darwin.apple_sdk.frameworks.Cocoa ];

                  packages.realworld-haskell = {
                    # NOTE: https://github.com/input-output-hk/haskell.nix/issues/1165
                    # flags = lib.genAttrs cabalFlags (exe: lib.mkOverride 10 true);
                    ghcOptions = [
                      # "-j${builtins.toString threads}"
                      "-j"
                      # "-O${builtins.toString optimize}"
                      "-O2"
                      # "+RTS ${RTS} -RTS"
                      "+RTS -N -A128m -n2m -RTS"
                    ];
                    components.exes = pkgs.lib.genAttrs [
                      (if builtins.elem exe frontend-exes then
                        "frontend"
                      else
                        "backend")
                    ] (_: { dontStrip = false; });
                  };
                }];
              };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flakes = pkgs.lib.genAttrs supported-compilers (compiler:
          let helper = exe: (pkgs.realworld-haskell-helper compiler exe).flake;
          in pkgs.lib.genAttrs [ "in-mem" "rel8" "vty" ] (exe: helper exe { })
          // {
            js = helper "js" {
              # This adds support for `nix build .#js-unknown-ghcjs-cabal:hello:exe:hello`
              # FIXME
              crossPlatforms = p: [ p.ghcjs ];
            };
          });
        packagesAndApps = pkgs.lib.genAttrs [ "apps" "packages" ] (key:
          let
            value = pkgs.lib.genAttrs supported-compilers (compiler:
              pkgs.lib.genAttrs exes (exe:
                flakes."${compiler}"."${exe}"."${key}"."${name}:exe:${
                  if builtins.elem exe frontend-exes then
                    "frontend"
                  else
                    "backend"
                }"));
            value2 = pkgs.lib.genAttrs exes (exe:
              value."ghc${
                if builtins.elem exe frontend-exes then "8107" else "922"
              }"."${exe}");
          in value // value2 // { default = value2.in-mem; });
        devShells = pkgs.lib.genAttrs supported-compilers (compiler:
          pkgs.lib.genAttrs exes
          (exe: flakes."${compiler}"."${exe}".devShells.default));
      in flakes.ghc922.in-mem // packagesAndApps // {
        devShells = devShells // { default = devShells.ghc922.in-mem; };
      });

}
