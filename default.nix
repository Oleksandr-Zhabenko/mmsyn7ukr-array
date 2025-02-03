{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.mkDerivation {
  pname = "mmsyn7ukr-array";
  version = "0.3.0.0";
  src = ./.;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = with pkgs.haskellPackages; [
    base directory end-of-exe mmsyn2-array mmsyn7ukr-common process
  ];
  homepage = "https://hackage.haskell.org/package/mmsyn7ukr-array";
  description = "A simple reduced basic interface to some SoX functionality or to produce a voice that can be used by mmsyn7h-array, dobutokO2-array and other similar packages";
  license = pkgs.lib.licenses.mit;
}
