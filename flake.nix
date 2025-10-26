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
        packages = with pkgs; [ emacs-nox ripgrep git coreutils ];
        shellHook = ''
          echo "Run tests: emacs -Q --batch -L lisp -l test/ert-runner.el"
          echo "Or via flake app: nix run .#tests"
          echo "Build (byte-compile): nix run .#build"
        '';
      };
    });

    apps = forAllSystems (pkgs:
      let
        emacs = pkgs.emacs-nox;

        testsDrv = pkgs.writeShellApplication {
          name = "carriage-tests";
          runtimeInputs = [ pkgs.ripgrep pkgs.git emacs ];
          text = ''
            set -euo pipefail
            exec ${emacs}/bin/emacs -Q --batch -L ${./lisp} -l ${./test}/ert-runner.el
          '';
        };

        buildDrv = pkgs.writeShellApplication {
          name = "carriage-build";
          runtimeInputs = [ pkgs.coreutils emacs ];
          text = ''
            set -euo pipefail
            tmp="$(mktemp -d)"
            cp -r ${./lisp} "$tmp/lisp"
            cd "$tmp"
            ${emacs}/bin/emacs -Q --batch \
              -eval "(setq byte-compile-error-on-warn t)" \
              -L lisp \
              -f batch-byte-compile lisp/*.el
            echo "Byte-compiled files are in: $tmp/lisp (temporary dir)"
          '';
        };
      in rec {
        tests = { type = "app"; program = "${testsDrv}/bin/carriage-tests"; };
        build = { type = "app"; program = "${buildDrv}/bin/carriage-build"; };
        default = tests;
      });

    checks = forAllSystems (pkgs: {
      ert = pkgs.runCommand "carriage-ert" { buildInputs = [ pkgs.emacs-nox pkgs.ripgrep pkgs.git ]; } ''
        cp -r ${./lisp} ./lisp
        cp -r ${./test} ./test
        ${pkgs.emacs-nox}/bin/emacs -Q --batch -L lisp -l test/ert-runner.el
        touch $out
      '';

      byte-compile = pkgs.runCommand "carriage-byte-compile" { buildInputs = [ pkgs.emacs-nox pkgs.coreutils ]; } ''
        cp -r ${./lisp} ./lisp
        # Compile with warnings as errors
        ${pkgs.emacs-nox}/bin/emacs -Q --batch \
          -eval "(setq byte-compile-error-on-warn t)" \
          -L lisp \
          -f batch-byte-compile lisp/*.el
        mkdir -p $out
        cp -r lisp $out/
      '';
    });
  };
}
