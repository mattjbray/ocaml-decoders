{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  buildInputs =
    [ pkgs.opam pkgs.pkg-config pkgs.gmp pkgs.zlib ]
    ++ (if pkgs.stdenv.isDarwin then
      (with pkgs.darwin.apple_sdk.frameworks; [ CoreServices Foundation ])
    else
      [ pkgs.inotify-tools ]);
}
