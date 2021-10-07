with import <nixpkgs> {};
mkShell {
    buildInputs = [
        feh
        lilypond
        shellcheck
        timidity
    ];
    shellHook = ''
        . .shellhook
    '';
}
