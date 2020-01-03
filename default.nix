let
  pkgs = import (import ./nixpkgs-pinned.nix) {};

  # Even though Nixpkgs ships a recent fonttools, pin to version 3.0.
  # Later versions produce fonts which don't load correctly in IE 9.
  fonttools = ps: ps.buildPythonPackage rec {
    pname = "fonttools";
    version = "3.0";
    src = ps.fetchPypi {
      inherit pname version;
      sha256 = "0f4iblpbf3y3ghajiccvdwk2f46cim6dsj6fq1kkrbqfv05dr4nz";
    };
  };
in
  pkgs.buildEnv {
    name = "blog-devenv";
    paths = [
      (pkgs.python38.withPackages (ps: [
        ps.brotli
        (fonttools ps)
      ]))
      pkgs.brotli
      pkgs.guetzli
      pkgs.mozjpeg
      pkgs.optipng
      pkgs.stack
      pkgs.zopfli
    ];
  }
