let
  default = import ./default.nix;
  pkgs = default.pkgs;
  shell = default.shell;

  commonBuildInputs = shell.buildInputs;
  commonConfigurePhase = ''
    cp ${./libraries.R} libraries.R
    mkdir -p $out
  '';

  # Function to create R derivations, without depends parameter
  makeRDerivation = { name, buildPhase }:
    let rdsFile = "${name}.rds";
    in pkgs.stdenv.mkDerivation {
      inherit name;
      buildInputs = commonBuildInputs;
      dontUnpack = true;
      configurePhase = commonConfigurePhase;
      inherit buildPhase;
      installPhase = ''
        cp ${rdsFile} $out/
      '';
    };

  # Define derivations
  mtcars_am = makeRDerivation {
    name = "mtcars_am";
    buildPhase = ''
      Rscript -e "
        source('libraries.R')
        mtcars_am <- filter(mtcars, am==1)
        saveRDS(mtcars_am, 'mtcars_am.rds')"
    '';
  };

  mtcars_head = makeRDerivation {
    name = "mtcars_head";
    buildPhase = ''
      Rscript -e "
        source('libraries.R')
        mtcars_am <- readRDS('${mtcars_am}/mtcars_am.rds')
        mtcars_head <- filter(mtcars_am, am==1)
        saveRDS(mtcars_head, 'mtcars_head.rds')"
    '';
  };

  # Define a flat JSON mapping of derivation names to their output paths
  pathMappingJson = builtins.toJSON {
    mtcars_am = "${mtcars_am}/mtcars_am.rds";
    mtcars_head = "${mtcars_head}/mtcars_head.rds";
  };

  # Write the flat JSON to a file
  pathMapping = pkgs.writeText "pathMapping.json" pathMappingJson;

in
{
  inherit mtcars_am mtcars_head;
  inherit pathMapping;
  default = pathMapping;  # Set pathMapping as the default target
}