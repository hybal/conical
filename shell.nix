{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  name = "conical";

  packages = with pkgs; [
      zig_0_14
      llvmPackages_19.libllvm
  ];
}

