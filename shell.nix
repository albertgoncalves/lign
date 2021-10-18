with import <nixpkgs> {};
mkShell {
    buildInputs = [
        (with ocamlPackages; [
            ocaml
            ocp-indent
        ])
        feh
        lilypond
        rlwrap
        shellcheck
        timidity
    ];
    shellHook = ''
        . .shellhook
    '';
}
