{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.05";
  };

  outputs = { self, nixpkgs }:
    let
      # List of supported systems:
      supportedSystems = [ "x86_64-linux" ];

      # Function to generate a set based on supported systems:
      forAllSystems = f:
        nixpkgs.lib.genAttrs supportedSystems (system: f system);

      # Attribute set of nixpkgs for each system:
      nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; });
    in
    {
      packages = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system};
          haskell = pkgs.haskellPackages;
          hlib = pkgs.haskell.lib;
        in
        {
          # Full Haskell package with shared/static libraries:
          lib = haskell.callCabal2nix "display-control" "${self}/control" {
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

          # Just the inhibit-screensaver executable:
          bin = hlib.justStaticExecutables self.packages.${system}.lib;

          # The firmware to flash on the arduino:
          firmware = import ./arduino { inherit pkgs; };
        });

      defaultPackage = forAllSystems (system: self.packages.${system}.bin);

      overlay = final: prev: {
        pjones = (prev.pjones or { }) //
          { display-control = self.packages.${prev.system}.bin; };
      };

      devShell = forAllSystems (system:
        let pkgs = nixpkgsFor.${system}; in
        pkgs.mkShell {
          inherit (self.packages.${system}.firmware)
            ARDUINO_DIR
            ARDMK_DIR;

          buildInputs = with pkgs; [
            haskellPackages.cabal-install
            haskellPackages.hlint
          ];

          inputsFrom = builtins.attrValues self.packages.${system};
        });
    };
}
