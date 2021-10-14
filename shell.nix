with import <nixpkgs> {};
mkShell {
    buildInputs = [
        (with ocamlPackages; [
            ocaml
            ocp-indent
        ])
        feh
        lilypond
        ocaml
        shellcheck
        timidity
    ];
    shellHook = ''
        . .shellhook
    '';
}
