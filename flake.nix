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
        devShell = pkgs.mkShell {
          buildInputs = with pkgs;
            [
              nil
              alejandra
              opam
              ocamlPackages.ocaml
              ocamlPackages.dune_3
              ocamlPackages.lsp
              ocamlPackages.batteries
              ocamlPackages.alcotest
              ocamlPackages.findlib
              ocamlPackages.ocamlformat
              wabt
              wasmtime
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
        packages.wascaml = pkgs.ocamlPackages.buildDunePackage {
          pname = "wascaml";
          version = "0.1.0";
          src = ./.;
          duneVersion = "3";
          doCheck = true;
          checkInputs = with pkgs; [
            ocamlPackages.alcotest
          ];
        };
        packages.default = packages.wascaml;
        apps.${system}.default = {
          type = "app";
          program = "${self.packages.default}/bin/wascaml.app";
        };
      }
    );
}
