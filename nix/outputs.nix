{ repoRoot, inputs, pkgs, system, lib }:
let
  project = repoRoot.nix.project;
in
[
  (project.flake)
  { }
]
