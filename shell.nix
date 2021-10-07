with import <nixpkgs> {};
mkShell.override { stdenv = llvmPackages_12.stdenv; } {
    buildInputs = [
        feh
        lilypond
        llvmPackages_12.lld
        shellcheck
        timidity
    ];
    shellHook = ''
        . .shellhook
    '';
}
