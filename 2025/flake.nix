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
        p.hspec
        p.hspec-megaparsec
        p.megaparsec
        p.parallel
        p.parser-combinators
        p.tasty
        p.tasty-hspec
        p.vector
        p.text
        p.logict
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
            haskellPkgs.haskell-language-server
          ];
        };
        packages.${system} = {
          day01 = haskellPkgs.callPackage ./day01 {};
          day02 = haskellPkgs.callPackage ./day02 {};
          day03 = haskellPkgs.callPackage ./day03 {};
          day04 = haskellPkgs.callPackage ./day04 {};
          day05 = haskellPkgs.callPackage ./day05 {};
        };
      };
}
