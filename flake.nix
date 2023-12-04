
# {
#   inputs = {
#     nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
#     flake-utils.url = "github:numtide/flake-utils";
#     easy-purescript-nix.url = "github:justinwoo/easy-purescript-nix";
#   };

#   outputs = { nixpkgs, flake-utils, easy-purescript-nix, ... }:
#     flake-utils.lib.eachDefaultSystem (system:
#       let
#         pkgs = nixpkgs.legacyPackages.${system};
#         easy-ps = easy-purescript-nix.packages.${system};
#       in
#       {
#         devShells = {
#           default = pkgs.mkShell {
#             name = "purescript-custom-shell";
#             buildInputs = [
#               easy-ps.purs-0_15_8
#               easy-ps.spago
#               easy-ps.purescript-language-server
#               easy-ps.purs-tidy
#               pkgs.nodejs-18_x
#               pkgs.esbuild
#             ];
#             shellHook = ''
#               source <(spago --bash-completion-script `which spago`)
#               source <(node --completion-bash)
#               '';
#           };
#        };
#      }
#   );
# }
  
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    flake-utils.url  = "github:numtide/flake-utils";
    purescript-overlay.url = "github:thomashoneyman/purescript-overlay";
    purescript-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, purescript-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ purescript-overlay.overlays.default ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
      in {
        devShells.default = pkgs.mkShell {
          # You now have access to the standard PureScript toolchain in pkgs
          buildInputs = [
            pkgs.purs
            pkgs.spago-unstable
            pkgs.purs-tidy-bin.purs-tidy-0_10_0
            pkgs.purs-backend-es
          ];
        };
      }
    );
}
