# {
#   outputs = { self, nixpkgs }:
#     let
#       supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
#       forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
#       nixpkgsFor = forAllSystems (system: import nixpkgs {
#         inherit system;
#         overlays = [ self.overlay ];
#       });
#     in
#     {
#       overlay = (final: prev: {
#         haskell-hello = final.haskellPackages.callCabal2nix "haskell-hello" ./. {};
#       });
#       packages = forAllSystems (system: {
#          haskell-hello = nixpkgsFor.${system}.haskell-hello;
#       });
#       defaultPackage = forAllSystems (system: self.packages.${system}.haskell-hello);
#       checks = self.packages;
#       devShell = forAllSystems (system: let haskellPackages = nixpkgsFor.${system}.haskellPackages;
#         in haskellPackages.shellFor {
#           packages = p: [self.packages.${system}.haskell-hello];
#           withHoogle = true;
#           buildInputs = with haskellPackages; [
#             haskell-language-server
#             ghcid
#             cabal-install
#           ];
#         # Change the prompt to show that you are in a devShell
#         shellHook = "export PS1='\\e[1;34mdev > \\e[0m'";
#         });
#   };
# }

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
        overlays = [
          haskellNix.overlay
          (final: prev: {
            # This overlay adds our project to pkgs
            realworld-haskell-helper = compiler-nix-name: flag:
              final.haskell-nix.project' {
                src = prev.haskell-nix.haskellLib.cleanGit {
                  name = "realworld-haskell";
                  src = ./.;
                };
                inherit compiler-nix-name;

                cabalProjectFileName = pkgs.lib.mkForce ("cabal.project"
                  + pkgs.lib.optionalString (flag != null) ".${flag}");
                # This is used by `nix develop .`
                shell = {
                  # to open a shell for use with
                  # `cabal`, `hlint` and `haskell-language-server`
                  tools = {
                    cabal = { };
                    hlint = { };
                    haskell-language-server = { };
                  };
                  # Non-Haskell shell tools go here
                  buildInputs = with pkgs; [ nixpkgs-fmt ];
                  # This adds `js-unknown-ghcjs-cabal` to the shell.
                  # FIXME
                  # crossPlatforms = p:
                  #   # [ p.ghcjs ];
                  #   pkgs.lib.optionals (flag == "frontend-js") [ p.ghcjs ];
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
                    # flags = lib.genAttrs cabalFlags (flag: lib.mkOverride 10 true);
                    ghcOptions = [
                      # "-j${builtins.toString threads}"
                      "-j"
                      # "-O${builtins.toString optimize}"
                      "-O2"
                      # "+RTS ${RTS} -RTS"
                      "+RTS -N -A128m -n2m -RTS"
                    ];
                    components.exes =
                      # pkgs.lib.genAttrs [ "frontend" "backend" ]
                      pkgs.lib.genAttrs [ "frontend" ]
                      (_: { dontStrip = false; });
                  };
                }];
              };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flakes = with pkgs; {
          default = (realworld-haskell-helper "ghc8107" null).flake { };
          backend-rel8 =
            (realworld-haskell-helper "ghc8107" "backend-rel8").flake { };
          frontend-js =
            (realworld-haskell-helper "ghc8107" "frontend-js").flake {
              # This adds support for `nix build .#js-unknown-ghcjs-cabal:hello:exe:hello`
              # FIXME
              crossPlatforms = p: [ p.ghcjs ];
            };
        };
      in flakes.default // rec {
        # Built by `nix build .`
        defaultPackage =
          flakes.default.packages."realworld-haskell:exe:backend";
        packages = flakes.default.packages // {
          "realworld-haskell:exe:frontend-js" =
            flakes.frontend-js.packages."realworld-haskell:exe:frontend";
          "realworld-haskell:exe:backend-rel8" =
            flakes.backend-rel8.packages."realworld-haskell:exe:backend";
        };
        devShell = devShells."default";
        devShells = builtins.listToAttrs (builtins.map (flag: {
          name = flag;
          value = flakes."${flag}".devShell;
        }) [ "default" "frontend-js" "backend-js" ]);
      });
  # in flakes.frontend-js // {
  #   defaultPackage =
  #     flakes.frontend-js.packages."realworld-haskell:exe:frontend";
  # });

}
