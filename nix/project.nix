{ repoRoot, inputs, pkgs, lib, system }:

let

  cabalProject = pkgs.haskell-nix.cabalProject' ({ config, pkgs, ... }: {
    name = "plutus-plonk-example";
    src = ../.;
    compiler-nix-name = "ghc96";
    shell.withHoogle = false;
    inputMap = {
      "https://chap.intersectmbo.org/" = inputs.iogx.inputs.CHaP;
    };
  });

  project = lib.iogx.mkHaskellProject {
    inherit cabalProject;
    shellArgs = repoRoot.nix.shell;
  };

in

project
