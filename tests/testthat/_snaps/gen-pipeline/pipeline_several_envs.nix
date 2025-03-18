let
  default = import ./default.nix;
  defaultPkgs = default.pkgs;
  defaultShell = default.shell;
  defaultBuildInputs = defaultShell.buildInputs;
  defaultConfigurePhase = ''
    cp ${./_rixpress/default_libraries.R} libraries.R
    mkdir -p $out
  '';
tempdir_default2 = import ./tempdir/default2.nix;
  tempdir_default2Pkgs = tempdir_default2.pkgs;
  tempdir_default2Shell = tempdir_default2.shell;
  tempdir_default2BuildInputs = tempdir_default2Shell.buildInputs;
  tempdir_default2ConfigurePhase = ''
    cp ${./_rixpress/tempdir_default2_libraries.R} libraries.R
    mkdir -p $out
  '';
  
  # Function to create R derivations
  makeRDerivation = { name, buildInputs, configurePhase, buildPhase, src ? null }:
    let rdsFile = "${name}.rds";
    in defaultPkgs.stdenv.mkDerivation {
      inherit name src;
      dontUnpack = true;
      inherit buildInputs configurePhase buildPhase;
      installPhase = ''
        cp ${rdsFile} $out/
      '';
    };

  # Define all derivations
    mtcars_am = makeRDerivation {
    name = "mtcars_am";
    buildInputs = defaultBuildInputs;
    configurePhase = defaultConfigurePhase;
    buildPhase = ''
      Rscript -e "
        source('libraries.R')
        mtcars_am <- dplyr::filter(mtcars, am == 1)
        saveRDS(mtcars_am, 'mtcars_am.rds')"
    '';
  };

  mtcars_head = makeRDerivation {
    name = "mtcars_head";
    buildInputs = tempdir_default2BuildInputs;
    configurePhase = tempdir_default2ConfigurePhase;
    buildPhase = ''
      Rscript -e "
        source('libraries.R')
        mtcars_am <- readRDS('${mtcars_am}/mtcars_am.rds')
        mtcars_head <- head(mtcars_am)
        saveRDS(mtcars_head, 'mtcars_head.rds')"
    '';
  };

  # Generic default target that builds all derivations
  allDerivations = defaultPkgs.symlinkJoin {
    name = "all-derivations";
    paths = with builtins; attrValues { inherit mtcars_am mtcars_head; };
  };

in
{
  inherit mtcars_am mtcars_head;
  default = allDerivations;
}
