let
  pkgs = import ./nixpkgs-pinned.nix {};

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

  ghc = pkgs.ghc.withPackages (ps: [
    ps.async
    ps.containers
    ps.pandoc
    ps.tagsoup
    ps.text
  ]);
  
  # Build the static site generator directly and make it part of the development
  # environment.
  blog = pkgs.stdenv.mkDerivation rec {
    name = "${pname}-${version}";
    pname = "blog";
    version = "3.1.0";

    src = ./src;
    
    # For the run time options, use 4 threads (-N4), and use a heap of 256 MiB
    # (-H). These settings were found to be optimal by running ghc-gc-tune.
    ghcOptions = [
      "-Wall"
      "-fwarn-tabs"
      "-O3"
      "-threaded"
      "-rtsopts \"-with-rtsopts=-N4 -A8388608 -H268435456\""
    ];

    buildPhase = ''
      ${ghc}/bin/ghc -o blog -outputdir . "$ghcOptions" $src/*.hs
    '';
    installPhase = ''
      mkdir -p $out/bin
      cp blog $out/bin
    '';
  };
in
  pkgs.buildEnv {
    name = "blog-devenv";
    paths = [
      blog
      # Include GHC so we can still hack on the generator without having to run
      # "nix build" all the time.
      ghc
      # And the runtime dependencies of the generator and utilities.
      (pkgs.python38.withPackages (ps: [
        ps.brotli
        (fonttools ps)
      ]))
      pkgs.brotli
      pkgs.guetzli
      pkgs.m4
      pkgs.mozjpeg
      pkgs.optipng
      pkgs.zopfli
    ];
  }
