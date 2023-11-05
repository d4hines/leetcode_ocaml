{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
    nixpkgs.url = "github:nix-ocaml/nix-overlays";
    nixpkgs.inputs.flake-utils.follows = "flake-utils";
    nixpkgs.inputs.nix-filter.follows = "nix-filter";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    nix-filter,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            # add overlays here
          ];
        };
        pkgsCross = pkgs.pkgsCross.musl64;

        camlchat = {
          pkgs,
          ocamlPackages,
          static ? false,
        }:
          with ocamlPackages;
            buildDunePackage {
              pname = "camlchat";
              version = "0.1.0";
              src = nix-filter.lib.filter {
                root = ./.;
                include = [
                  "src"
                  "dune-project"
                  "camlchat.opam"
                ];
              };
              buildPhase = ''
                runHook preBuild
                dune build --display=short --profile=${
                  if static
                  then "static"
                  else "release"
                }
                runHook postBuild
              '';
              installPhase = ''
                mkdir -p $out/bin
                cp "$(readlink -f _build/install/default/bin/camlchat)" "$out/bin/camlchat"
              '';
              propagatedBuildInputs = [
                ppx_deriving
                cmdliner
                ppx_deriving_cmdliner
                ppx_expect
              ];
            };
      in {
        packages.default = pkgs.callPackage camlchat {
          ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_1;
        };
        packages.static = pkgsCross.callPackage camlchat {
          static = true;
          ocamlPackages = pkgsCross.ocaml-ng.ocamlPackages_5_1;
        };
        devShell = pkgs.mkShell {
          inputsFrom = [self.packages.${system}.default];
          buildInputs = with pkgs;
          with ocaml-ng.ocamlPackages_5_1; [
            ocaml-lsp
            ocamlformat
            odoc
            ocaml
            dune_3
            nixfmt
            utop
          ];
        };
      }
    );
}
