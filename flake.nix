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
        supported-ghc9s = [
          "922"
          # TEMP FIXME https://github.com/haskell/haskell-language-server/issues/2985
          "923"
        ];
        supported-ghc8s = [ "8107" ];
        supported-ghcs = supported-ghc8s ++ supported-ghc9s;
        backend-exes = [ "in-mem" "rel8" ];
        frontend-exes = [ "js" "vty" "warp" "native" ];
        exes = backend-exes ++ frontend-exes;
        frontOrBack = exe:
          if builtins.elem exe backend-exes then "backend" else "frontend";
        isGhc8 = compiler: builtins.elem compiler supported-ghc8s;
        isGhc9 = compiler: builtins.elem compiler supported-ghc9s;
        name = "realworld-haskell";
        overlays = [
          haskellNix.overlay
          (final: prev: {
            # This overlay adds our project to pkgs
            realworld-haskell-helper = ghcVersion: exe:
              assert pkgs.lib.assertOneOf "exe" exe exes
                && pkgs.lib.assertOneOf "ghcVersion" ghcVersion supported-ghcs;
              final.haskell-nix.project' {
                src = prev.haskell-nix.haskellLib.cleanGit {
                  inherit name;
                  src = ./.;
                };
                compiler-nix-name = "ghc${ghcVersion}";

                cabalProjectFileName =
                  pkgs.lib.mkForce "cabal.project.${frontOrBack exe}-${exe}";
                # This is used by `nix develop .`
                shell = {
                  # to open a shell for use with
                  # `cabal`, `hlint` and `haskell-language-server`
                  tools = {
                    cabal = { };
                    hlint = { };
                    ormolu = { };
                    # TEMP FIXME https://github.com/phadej/cabal-fmt/issues/44
                    cabal-fmt = pkgs.lib.optionalAttrs (isGhc9 ghcVersion) {
                      version = "latest";
                      cabalProject = ''
                        packages: .
                        source-repository-package
                            type: git
                            location: https://github.com/phadej/cabal-fmt.git
                            tag: 6651ffdccdfce71330f2b5cde9f8f23b616abf82
                            --sha256: 19zyq6ailzhfcqy2zfqdd64sv0jlbcz8bbs1ag769m6v55dlrq59
                        allow-newer: cabal-fmt-0.1.5.1:base
                        allow-newer: cabal-fmt-0.1.5.1:bytestring
                      '';
                    };
                    ghcid = { };
                    # TEMP FIXME https://github.com/haskell/haskell-language-server/issues/2179
                    # TEMP FIXME https://github.com/input-output-hk/haskell.nix/issues/1272
                    haskell-language-server =
                      pkgs.lib.optionalAttrs (isGhc9 ghcVersion) {
                        version = "latest";
                        cabalProject = ''
                          packages: .
                          package haskell-language-server
                          flags: -haddockComments
                        '';
                      };
                  } // pkgs.lib.optionalAttrs (isGhc8 ghcVersion) {
                    stan = { };
                  };
                  # Non-Haskell shell tools go here
                  buildInputs = with pkgs; [ nixpkgs-fmt sqls ];
                  # This adds `js-unknown-ghcjs-cabal` to the shell.
                  # FIXME
                  crossPlatforms = p:
                    pkgs.lib.optionals (exe == "js") [ p.ghcjs ];
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
                    components.exes = pkgs.lib.genAttrs [ (frontOrBack exe) ]
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
        flakes = pkgs.lib.genAttrs supported-ghcs (compiler:
          let helper = exe: (pkgs.realworld-haskell-helper compiler exe).flake;
          in pkgs.lib.genAttrs [ "in-mem" "rel8" "vty" "warp" ]
          (exe: helper exe { }) // {
            js = helper "js" {
              # This adds support for `nix build .#js-unknown-ghcjs-cabal:hello:exe:hello`
              # FIXME
              crossPlatforms = p: [ p.ghcjs ];
            };
          });
        addShortcuts = result:
          result // pkgs.lib.genAttrs exes (exe:
            result."${if builtins.elem exe frontend-exes then
              "8107"
            else
              "922"}"."${exe}");
        appsAndPackages = pkgs.lib.genAttrs [ "apps" "packages" ] (key:
          addShortcuts (pkgs.lib.genAttrs supported-ghcs (compiler:
            pkgs.lib.genAttrs exes (exe:
              flakes."${compiler}"."${if exe == "native" then
                "js"
              else
                exe}"."${key}"."${
                pkgs.lib.optionalString (exe == "js") "js-unknown-ghcjs:"
              }${name}:exe:${frontOrBack exe}"))));
        shells = {
          devShells = addShortcuts (pkgs.lib.genAttrs supported-ghcs (compiler:
            pkgs.lib.genAttrs exes (exe:
              flakes."${compiler}"."${if exe == "native" then
                "js"
              else
                exe}".devShell)));
        };
      in appsAndPackages // shells);
}
