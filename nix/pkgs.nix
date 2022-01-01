let
  sources = import ./sources.nix { };
  haskellNix = import sources.haskellNix { };
in import haskellNix.sources.nixpkgs-2111 haskellNix.nixpkgsArgs // {
  inherit (import sources.nixpkgs { }) sqls;
}
