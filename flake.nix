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
      trivial = nixpkgs.lib.trivial;
      haskell-lib = nixpkgs.haskell.lib;
      backend-project = devTools:
       let addBuildTools = (trivial.flip haskell-lib.addBuildTools) devTools;
       in nixpkgs.haskellPackages.developPackage {
         root = nixpkgs.lib.sourceFilesBySuffices ./server [ ".cabal" ".hs" ];
         name = "fitness-server-backend";
         returnShellEnv = !(devTools == [ ]);
         modifier = (trivial.flip trivial.pipe) [
           addBuildTools
           haskell-lib.dontHaddock
           haskell-lib.enableStaticLibraries
           haskell-lib.justStaticExecutables
           haskell-lib.disableLibraryProfiling
           haskell-lib.disableExecutableProfiling
         ];
       };
    in
    {
      packages.backend = backend-project [ ];
      packages.frontend = nixpkgs.stdenv.mkDerivation {
        name = "fitness-tracker-frontend";
        src = nixpkgs.nix-gitignore.gitignoreSource [ ".git" ] ./frontend;
        nativeBuildInputs = [
            easy-purescript-nix.purs-0_14_0
        ] ++ (
          spago2nix.packages.${system}.spago2nix_nativeBuildInputs {
            srcs-dhall = [./frontend/spago.dhall ./frontend/packages.dhall];
          }
        );
        unpackPhase = ''
          cp -r $src/src .
          install-spago-style
          '';
        buildPhase = ''
          build-spago-style "./src/**/*.purs"
          '';
        installPhase = ''
          mkdir -p $out
          mv output $out/
          '';
      };
  });
}
