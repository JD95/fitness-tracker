{
  description = "fitness trakcer frontend";

  inputs = {
      nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
      flake-utils.url = "github:numtide/flake-utils";
      spago2nix.url = "github:justinwoo/spago2nix";
      easy-purescript-nix = {
        url = "github:justinwoo/easy-purescript-nix";
        flake = false;
      };
  };

  outputs = { self, spago2nix, flake-utils, ... }@inputs:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
    let
      nixpkgs = inputs.nixpkgs.legacyPackages.${system};
      easy-purescript-nix = import inputs.easy-purescript-nix {pkgs = nixpkgs;};
    in
    {
      packages.default = nixpkgs.stdenv.mkDerivation {
        name = "fitness-tracker-frontend";
        src = nixpkgs.nix-gitignore.gitignoreSource [ ".git" ] ./.;
        nativeBuildInputs = [
            easy-purescript-nix.purs-0_14_0
        ] ++ (
          spago2nix.packages.${system}.spago2nix_nativeBuildInputs {
            srcs-dhall = [./spago.dhall ./packages.dhall];
          }
        );
        unpackPhase = ''
          cp -r $src/src .
          cp $src/style.css .
          cp $src/index.html .
          install-spago-style
          '';
        buildPhase = ''
          build-spago-style "./src/**/*.purs"
          '';
        installPhase = ''
          mkdir -p $out
          mv output $out/
          mv index.html $out/
          mv style.css $out/
          '';
      };
  });
}
