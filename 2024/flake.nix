{
  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      lib = pkgs.lib;
      stdenv = pkgs.stdenv;
      haskellPkgs = pkgs.haskell.packages.ghc912;
      builder = haskellPkgs.ghcWithPackages (p: [
        p.file-embed
        p.megaparsec
        p.parser-combinators
      ]);
    in
      {
        formatter.${system} = pkgs.nixfmt-rfc-style;
        devShells.${system}.default = pkgs.mkShell {
          packages = [
            builder
            haskellPkgs.cabal-install
            haskellPkgs.cabal2nix
            haskellPkgs.ghcid
            haskellPkgs.ormolu
          ];
        };
        packages.${system} = {
          day01 = haskellPkgs.callPackage ./day01 {};
        };
      };
}
