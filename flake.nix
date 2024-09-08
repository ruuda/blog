{
  description = "Blog";

  inputs.nixpkgs.url = "nixpkgs/nixos-24.05";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };

      ghc = pkgs.ghc.withPackages (ps: [
        ps.async
        ps.containers
        ps.pandoc
        ps.tagsoup
        ps.text
      ]);

      # Build the static site generator directly and make it part of the
      # development environment.
      blog = pkgs.stdenv.mkDerivation rec {
        name = "${pname}-${version}";
        pname = "blog";
        version = "3.1.0";

        src = ./src;
        
        # For the run time options, use 4 threads (-N4), and use a heap of
        # 256 MiB (-H). These settings were found to be optimal by running
        # ghc-gc-tune.
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
      {
        packages."${system}".default = blog;

        devShells."${system}".default = pkgs.mkShell {
          name = "blog";
          nativeBuildInputs = [
            blog
            # Include GHC so we can still hack on the generator without having
            # to run "nix build" all the time.
            ghc
            # And the runtime dependencies of the generator and utilities.
            (pkgs.python39.withPackages (ps: [
              ps.brotli
              ps.fontforge
              ps.fonttools
            ]))
            pkgs.brotli
            pkgs.guetzli
            pkgs.m4
            pkgs.mozjpeg
            pkgs.optipng
            pkgs.scour
            pkgs.zopfli
          ];
        };
      };
}
