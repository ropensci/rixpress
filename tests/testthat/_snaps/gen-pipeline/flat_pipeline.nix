let
  default = import ./default.nix;
  pkgs = default.pkgs;
  shell = default.shell;

  commonBuildInputs = shell.buildInputs;
  commonConfigurePhase = ''
    cp ${./libraries.R} libraries.R
    mkdir -p $out
  '';

  # Function to create R derivations
  makeRDerivation = { name, buildPhase, src ? null }:
    let rdsFile = "${name}.rds";
    in pkgs.stdenv.mkDerivation {
      inherit name src;
      buildInputs = commonBuildInputs;
      dontUnpack = true;
      configurePhase = commonConfigurePhase;
      inherit buildPhase;
      installPhase = ''
        cp ${rdsFile} $out/
      '';
  };

  # Define all derivations
  mtcars_am = makeRDerivation {
    name = "mtcars_am";
    buildPhase = ''
      Rscript -e "
        source('libraries.R')
        mtcars_am <- dplyr::filter(mtcars, am == 1)
        saveRDS(mtcars_am, 'mtcars_am.rds')"
    '';
  };

  mtcars_head = makeRDerivation {
    name = "mtcars_head";
    buildPhase = ''
      Rscript -e "
        source('libraries.R')
        mtcars_head <- head(mtcars_am)
        saveRDS(mtcars_head, 'mtcars_head.rds')"
    '';
  };

  # Generic default target that builds all derivations
  allDerivations = pkgs.symlinkJoin {
    name = "all-derivations";
    paths = with builtins; attrValues { inherit mtcars_am mtcars_head; };
  };

in
{
  inherit mtcars_am mtcars_head;  # Make individual derivations available as attributes
  default = allDerivations;  # Set the default target to build everything
}
