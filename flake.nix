{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskell = pkgs.haskellPackages;
      in
      {
        defaultPackage = self.packages.${system}.control;

        packages.control = haskell.callCabal2nix "display-control" "${self}/control" {
          relude = haskell.relude_1_0_0_1;
          serialport =
            let
              src = {
                pkg = "serialport";
                ver = "0.5.1";
                sha256 = "1dx5gz48kal805sl47kh8vd83gs55knqx1incm7b2xdpsyg1sb0a";
              };
              drv = haskell.callHackageDirect src { };
            in
            pkgs.haskell.lib.dontCheck drv;
        };

        packages.firmware = import ./arduino { inherit pkgs; };

        devShell = pkgs.mkShell {
          inherit (self.packages.${system}.firmware)
            ARDUINO_DIR
            ARDMK_DIR;

          buildInputs = [
            haskell.cabal-install
            haskell.hlint
          ];

          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}
