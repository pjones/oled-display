{ sources ? import ../nix/sources.nix
, pkgs ? import sources.nixpkgs { }
, nix-hs ? import sources.nix-hs { inherit pkgs; }
, ghc ? "default"
}:

nix-hs {
  cabal = ./display-control.cabal;
  compiler = ghc;

  overrides = lib: self: super: {
    relude = super.relude_1_0_0_1;

    # Serial port 0.5.2 has strict bounds on bytestring 0.11 for some reason.
    serialport =
      let
        src = {
          pkg = "serialport";
          ver = "0.5.1";
          sha256 = "1dx5gz48kal805sl47kh8vd83gs55knqx1incm7b2xdpsyg1sb0a";
        };
        drv = super.callHackageDirect src { };
      in
      lib.dontCheck drv;
  };
}
