let
  pkgs = import (import ./nixpkgs-pinned.nix) {};
in
  # Needed for the locale-archive.
  pkgs.glibcLocales
