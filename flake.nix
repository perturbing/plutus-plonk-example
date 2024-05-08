{
  description = "A plutus plonk verifier using Circom and SnarkJS";

  inputs = {

    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    iohkNix.url = "github:input-output-hk/iohk-nix";
    flake-utils.url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";

    # this is node 8.10
    cardano-node.url = "github:IntersectMBO/cardano-node/11d12d8fb6a4d65a996884f283bb40d66d904bbf";

    CHaP.url = "github:IntersectMBO/cardano-haskell-packages?ref=repo";
    CHaP.flake = false;


    # the flakes to get circom and snarkjs
    blockchain-utils.url = "github:metacraft-labs/nix-blockchain-development";
    snarkjs-cardano.url = "github:perturbing/snarkjs-cardano/9c40e357378908bdfe657313a0af7152ba325b33";

    # non-flake nix compatibility
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
  };

  outputs = inputs:
    let
      profiling = false;
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
       ]; in
    inputs.flake-utils.lib.eachSystem supportedSystems (system:
      let
        # setup our nixpkgs with the haskell.nix overlays, and the iohk-nix
        # overlays...
        nixpkgs = import inputs.nixpkgs {
          overlays = [inputs.haskellNix.overlay] ++ builtins.attrValues inputs.iohkNix.overlays;
          inherit system;
          inherit (inputs.haskellNix) config;
        };

        # ... and construct a flake from the cabal.project file.
        # We use cabalProject' to ensure we don't build the plan for
        # all systems.
        flake = (nixpkgs.haskell-nix.cabalProject' rec {
          src = ./.;
          name = "plutus-plonk-example";
          # since we depend on plutus-tx-plugin, we have to use ghc96
          compiler-nix-name = "ghc96";

          # CHaP input map, so we can find CHaP packages (needs to be more
          # recent than the index-state we set!). Can be updated with
          #
          #  nix flake lock --update-input CHaP
          #
          inputMap = {
            "https://chap.intersectmbo.org/" = inputs.CHaP;
          };

          # tools we want in our shell
          shell.tools = {
            cabal = "3.10.3.0";
            ghcid = "0.8.8";
            haskell-language-server = "latest";
            hlint = {};
            # weeder = "2.4.1";
          };
          shell.shellHook = ''
            export REPO_ROOT="$(pwd)"
            export CARDANO_NODE_SOCKET_PATH="$REPO_ROOT/local-testnet/example/node-spo1/node.sock"
          '';
          # Now we use pkgsBuildBuild, to make sure that even in the cross
          # compilation setting, we don't run into issues where we pick tools
          # for the target.
          shell.buildInputs = with nixpkgs.pkgsBuildBuild; [
            # add deno for front end for now, might switch to nodejs
            jq
            # add cardano-node and client to shell for running local testnets
            inputs.cardano-node.outputs.packages.${system}.cardano-node
            inputs.cardano-node.outputs.packages.${system}.cardano-cli
            # add circom and snarkjs to shell
            inputs.blockchain-utils.outputs.packages.${system}.circom
            inputs.snarkjs-cardano.defaultPackage.${system}

            (pkgs.writeShellScriptBin "deploy-local-testnet" ''
               cd $REPO_ROOT
               cd local-testnet
               scripts/babbage/mkfiles.sh
               example/run/all.sh
            '')

            (pkgs.writeShellScriptBin "purge-local-testnet" ''
               cd $REPO_ROOT
               rm -Rf local-testnet/logs
               rm -Rf local-testnet/example
            '')
          ];
          shell.withHoogle = true;

          # package customizations as needed. Where cabal.project is not
          # specific enough, or doesn't allow setting these.
          modules = [
            ({pkgs, ...}: {
              # Packages we wish to ignore version bounds of.
              # This is similar to jailbreakCabal, however it
              # does not require any messing with cabal files.
              packages.katip.doExactConfig = true;

              # split data output for ekg to reduce closure size
              packages.ekg.components.library.enableSeparateDataOutput = true;
              packages.cardano-binary.configureFlags = [ "--ghc-option=-Werror" ];
              packages.cardano-crypto-class.configureFlags = [ "--ghc-option=-Werror" ];
              packages.slotting.configureFlags = [ "--ghc-option=-Werror" ];
              enableLibraryProfiling = profiling;
            })
            ({pkgs, ...}: with pkgs; nixpkgs.lib.mkIf stdenv.hostPlatform.isWindows {
              packages.text.flags.simdutf = false;
              # Disable cabal-doctest tests by turning off custom setups
              packages.comonad.package.buildType = lib.mkForce "Simple";
              packages.distributive.package.buildType = lib.mkForce "Simple";
              packages.lens.package.buildType = lib.mkForce "Simple";
              packages.nonempty-vector.package.buildType = lib.mkForce "Simple";
              packages.semigroupoids.package.buildType = lib.mkForce "Simple";

              # Make sure we use a buildPackages version of happy
              # packages.pretty-show.components.library.build-tools = [ (pkgsBuildBuild.haskell-nix.tool compiler-nix-name "happy" "1.20.1.1") ];

              # Remove hsc2hs build-tool dependencies (suitable version will be available as part of the ghc derivation)
              packages.Win32.components.library.build-tools = lib.mkForce [];
              packages.terminal-size.components.library.build-tools = lib.mkForce [];
              packages.network.components.library.build-tools = lib.mkForce [];
            })
          ];
        }).flake (
          # we also want cross compilation to windows.
          nixpkgs.lib.optionalAttrs (system == "x86_64-linux") {
          crossPlatforms = p: [p.mingwW64];
        });
      in nixpkgs.lib.recursiveUpdate flake {
        # add a required job, that's basically all hydraJobs.
        hydraJobs = nixpkgs.callPackages inputs.iohkNix.utils.ciJobsAggregates
          { ciJobs = flake.hydraJobs; };
      }
    );

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      # drop this, once we stop needing it; when we have stable aarch64-darwin
      # builds
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
    allow-import-from-derivation = true;
  };
}
