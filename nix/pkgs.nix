{ nixpkgsPin ? "2111" }:
let
  sources = import ./sources.nix { };
  haskellNix = import sources.haskellNix { };
in import haskellNix.sources."nixpkgs-${nixpkgsPin}" haskellNix.nixpkgsArgs // {
  inherit (import sources.nixpkgs { }) sqls;
}
