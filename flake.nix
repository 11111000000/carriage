{
  description = "Carriage-mode - build dev shell and run ERT tests";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
  };

  outputs = { self, nixpkgs }:
  let
    systems = [ "x86_64-linux" "aarch64-linux" ];
    forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f (import nixpkgs { inherit system; }));
  in
  {
    devShells = forAllSystems (pkgs: {
      default = pkgs.mkShell {
        packages = with pkgs; [ emacs-nox ripgrep git ];
        shellHook = ''
          echo "Run tests: emacs -Q --batch -L lisp -l test/ert-runner.el"
          echo "Or via flake app: nix run .#tests"
        '';
      };
    });

    apps = forAllSystems (pkgs:
      let
        emacs = pkgs.emacs-nox;
        testsDrv = pkgs.writeShellApplication {
          name = "carriage-tests";
          runtimeInputs = [ pkgs.ripgrep pkgs.git ];
          text = ''
            set -euo pipefail
            exec ${emacs}/bin/emacs -Q --batch -L ${./lisp} -l ${./test}/ert-runner.el
          '';
        };
      in rec {
        tests = { type = "app"; program = "${testsDrv}/bin/carriage-tests"; };
        default = tests;
      });

    checks = forAllSystems (pkgs: {
      ert = pkgs.runCommand "carriage-ert" { buildInputs = [ pkgs.emacs-nox pkgs.ripgrep pkgs.git ]; } ''
        cp -r ${./lisp} ./lisp
        cp -r ${./test} ./test
        ${pkgs.emacs-nox}/bin/emacs -Q --batch -L lisp -l test/ert-runner.el
        touch $out
      '';
    });
  };
}
