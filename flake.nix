{
  description = "Ocaml WebAssembly toolings";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {inherit system;};
        frameworks = pkgs.darwin.apple_sdk.frameworks;
      in rec {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs;
            [
              nil
              alejandra
              opam
              ocamlPackages.ocaml
              ocamlPackages.dune_3
              ocamlPackages.ocaml-lsp
              ocamlPackages.batteries
              ocamlPackages.alcotest
              ocamlPackages.findlib
              ocamlPackages.ocamlformat
              ocamlPackages.odoc
              wabt
              wasmtime
              wasmer
            ]
            ++ lib.optional stdenv.isDarwin [
              frameworks.Security
              frameworks.CoreFoundation
              frameworks.CoreServices
            ];
          shellHook = ''
            eval $(opam env)
          '';
        };
        checks = {
          tests = pkgs.ocamlPackages.buildDunePackage {
            pname = "wascaml";
            version = "0.1.0";
            src = ./.;
            duneVersion = "3";
            doBuild = false;
            doCheck = true;
            checkInputs = with pkgs; [
              ocamlPackages.alcotest
            ];
            nativeCheckInputs = with pkgs; [
              wasmer
            ];
          };
        };
        packages.wascaml = pkgs.ocamlPackages.buildDunePackage {
          pname = "wascaml";
          version = "0.1.0";
          src = ./.;
          duneVersion = "3";
          doCheck = false;
        };
        packages.docs = pkgs.stdenv.mkDerivation {
          pname = "generate-dune-documents";
          version = "0.1.0";
          src = ./.;
          buildInputs = with pkgs; [
            ocamlPackages.ocaml
            ocamlPackages.dune_3
            ocamlPackages.odoc
            opam
          ];
          buildPhase = ''
            dune build @doc
          '';
          installPhase = ''
            mkdir -p $out/share/doc
            cp -r _build/default/_doc/_html/* $out/share/doc
          '';
        };
        packages.default = packages.wascaml;
        apps.${system}.default = {
          type = "app";
          program = "${self.packages.default}/bin/wascaml";
        };
      }
    );
}
