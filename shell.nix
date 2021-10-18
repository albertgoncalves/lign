with import <nixpkgs> {};
mkShell {
    buildInputs = [
        (with ocamlPackages; [
            ocaml
            ocp-indent
        ])
        feh
        lilypond
        shellcheck
        timidity
    ];
    shellHook = ''
        . .shellhook
    '';
}
