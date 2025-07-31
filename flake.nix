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
        
        # For the run time options, use 8 threads (-N8), and use an allocation
        # arena of 256 MiB (-A). These were found optimal after manual tuning.
        buildPhase = ''
          ${ghc}/bin/ghc -o blog \
            -outputdir . \
            -Wall \
            -fwarn-tabs \
            -O3 \
            -threaded \
            -rtsopts=all \
            -with-rtsopts="-N8 -A256M" \
            $src/*.hs
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
            pkgs.hivemind
            pkgs.m4
            pkgs.mozjpeg
            pkgs.optipng
            pkgs.scour
            pkgs.zopfli
          ];
        };
      };
}
