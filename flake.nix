{
  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";

    #ocamlformat 0.22.4 is a bit old, so is only in older versions of nixpkgs
    nixpkgs2305.url = "nixpkgs/nixos-23.05";

    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, nixpkgs2305, flake-utils }@attrs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        pkgs2305 = nixpkgs2305.legacyPackages.${system};
      in
      {
        # for use by nix fmt
        formatter = pkgs.nixpkgs-fmt;

        devShells.default =
          pkgs.mkShell {
            buildInputs =
              (if pkgs.stdenv.isDarwin then
                (with pkgs.darwin.apple_sdk.frameworks; [ CoreServices Foundation ])

              else
                [ pkgs.inotify-tools ]) ++ [
                pkgs.opam
                pkgs2305.ocamlformat_0_20_1
                #pkgs.pkg-config
              ]
            ;
          };

      });
}
