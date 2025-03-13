let
  default = import ./default.nix;
  pkgs = default.pkgs;
  shell = default.shell;

  commonBuildInputs = shell.buildInputs;
  commonConfigurePhase = ''
    cp ${./_rixpress/libraries.R} libraries.R
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
  mtcars = makeRDerivation {
    name = "mtcars";
    src = ./mtcars.csv;
    buildPhase = ''
      cp $src input_file
Rscript -e "
        source('libraries.R')
        data <- do.call(function(x) (read.csv(file = x, sep = '|')), list('input_file'))
        saveRDS(data, 'mtcars.rds')"
    '';
  };

  mtcars_am = makeRDerivation {
    name = "mtcars_am";
    buildPhase = ''
      Rscript -e "
        source('libraries.R')
        mtcars <- readRDS('${mtcars}/mtcars.rds')
        mtcars_am <- filter(mtcars, am == 1)
        saveRDS(mtcars_am, 'mtcars_am.rds')"
    '';
  };

  mtcars_head = makeRDerivation {
    name = "mtcars_head";
    buildPhase = ''
      Rscript -e "
        source('libraries.R')
        mtcars_am <- readRDS('${mtcars_am}/mtcars_am.rds')
        mtcars_head <- head(mtcars_am)
        saveRDS(mtcars_head, 'mtcars_head.rds')"
    '';
  };

  mtcars_tail = makeRDerivation {
    name = "mtcars_tail";
    buildPhase = ''
      Rscript -e "
        source('libraries.R')
        mtcars_head <- readRDS('${mtcars_head}/mtcars_head.rds')
        mtcars_tail <- tail(mtcars_head)
        saveRDS(mtcars_tail, 'mtcars_tail.rds')"
    '';
  };

  mtcars_mpg = makeRDerivation {
    name = "mtcars_mpg";
    buildPhase = ''
      Rscript -e "
        source('libraries.R')
        mtcars_tail <- readRDS('${mtcars_tail}/mtcars_tail.rds')
        mtcars_mpg <- select(mtcars_tail, mpg)
        saveRDS(mtcars_mpg, 'mtcars_mpg.rds')"
    '';
  };

  page = pkgs.stdenv.mkDerivation {
    name = "page";
    src = pkgs.lib.fileset.toSource {
      root = ./.;
      fileset = pkgs.lib.fileset.unions [ ./page.qmd ./content.qmd ./images ];
    };
    buildInputs = [ commonBuildInputs pkgs.which pkgs.quarto ];
    buildPhase = ''
  mkdir home
  export HOME=$PWD/home
  substituteInPlace page.qmd --replace-fail 'rxp_read("mtcars_head")' 'rxp_read("${mtcars_head}/mtcars_head.rds")'
  substituteInPlace page.qmd --replace-fail 'rxp_read("mtcars_tail")' 'rxp_read("${mtcars_tail}/mtcars_tail.rds")'
  substituteInPlace page.qmd --replace-fail 'rxp_read("mtcars_mpg")' 'rxp_read("${mtcars_mpg}/mtcars_mpg.rds")'
  quarto render page.qmd --output-dir $out
    '';
  };

  # Generic default target that builds all derivations
  allDerivations = pkgs.symlinkJoin {
    name = "all-derivations";
    paths = with builtins; attrValues { inherit mtcars mtcars_am mtcars_head mtcars_tail mtcars_mpg page; };
  };

in
{
  inherit mtcars mtcars_am mtcars_head mtcars_tail mtcars_mpg page;  # Make individual derivations available as attributes
  default = allDerivations;  # Set the default target to build everything
}
