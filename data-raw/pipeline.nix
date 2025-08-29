let
  default = import ./default.nix;
  defaultPkgs = default.pkgs;
  defaultShell = default.shell;
  defaultBuildInputs = defaultShell.buildInputs;
  defaultConfigurePhase = ''
    cp ${./_rixpress/default_libraries.R} libraries.R
    cp ${./_rixpress/default2_libraries.R} default2_libraries.R
    mkdir -p $out  
    mkdir -p .julia_depot  
    export JULIA_DEPOT_PATH=$PWD/.julia_depot  
    export HOME_PATH=$PWD
  '';
  

  default2 = import ./default2.nix;
  default2Pkgs = default2.pkgs;
  default2Shell = default2.shell;
  default2BuildInputs = default2Shell.buildInputs;
  default2ConfigurePhase = ''
    cp ${./_rixpress/default2_libraries.R} libraries.R
    mkdir -p $out  
    mkdir -p .julia_depot  
    export JULIA_DEPOT_PATH=$PWD/.julia_depot  
    export HOME_PATH=$PWD
  '';
  

  quarto_env = import ./quarto-env.nix;
  quarto_envPkgs = quarto_env.pkgs;
  quarto_envShell = quarto_env.shell;
  quarto_envBuildInputs = quarto_envShell.buildInputs;
  quarto_envConfigurePhase = ''
    cp ${./_rixpress/quarto_env_libraries.R} libraries.R
    mkdir -p $out  
    mkdir -p .julia_depot  
    export JULIA_DEPOT_PATH=$PWD/.julia_depot  
    export HOME_PATH=$PWD
  '';
  
  # Function to create R derivations
  makeRDerivation = { name, buildInputs, configurePhase, buildPhase, src ? null }:
    defaultPkgs.stdenv.mkDerivation {
      inherit name src;
      dontUnpack = true;
      inherit buildInputs configurePhase buildPhase;
      installPhase = ''
        cp ${name} $out/
      '';
    };

  # Define all derivations
    mtcars = makeRDerivation {
    name = "mtcars";
    src = defaultPkgs.lib.fileset.toSource {
      root = ./.;
      fileset = defaultPkgs.lib.fileset.unions [ ./mtcars.csv ];
    };
    buildInputs = defaultBuildInputs;
    configurePhase = defaultConfigurePhase;
    buildPhase = ''
      cp -r $src input_folder
Rscript -e "
source('libraries.R')
data <- do.call(function(x) (read.csv(file = x, sep = '|')), list('input_folder/mtcars.csv'))
saveRDS(data, 'mtcars')"
    '';
  };

  mtcars_am = makeRDerivation {
    name = "mtcars_am";
    buildInputs = default2BuildInputs;
    configurePhase = default2ConfigurePhase;
    buildPhase = ''
      Rscript -e "
        source('libraries.R')
        mtcars <- readRDS('${mtcars}/mtcars')
        mtcars_am <- filter(mtcars, TRUE)
        saveRDS(mtcars_am, 'mtcars_am')"
    '';
  };

  mtcars_head = makeRDerivation {
    name = "mtcars_head";
     src = defaultPkgs.lib.fileset.toSource {
      root = ./.;
      fileset = defaultPkgs.lib.fileset.unions [ ./my_head.R ];
    };
    buildInputs = defaultBuildInputs;
    configurePhase = defaultConfigurePhase;
    buildPhase = ''
            cp ${./my_head.R} my_head.R
      Rscript -e "
        source('libraries.R')
        mtcars_am <- readRDS('${mtcars_am}/mtcars_am')
                source('my_head.R')
        mtcars_head <- my_head(mtcars_am, 100)
        write.csv(mtcars_head, 'mtcars_head')"
    '';
  };

  mtcars_tail = makeRDerivation {
    name = "mtcars_tail";
     src = defaultPkgs.lib.fileset.toSource {
      root = ./.;
      fileset = defaultPkgs.lib.fileset.unions [ ./my_tail.R ];
    };
    buildInputs = defaultBuildInputs;
    configurePhase = defaultConfigurePhase;
    buildPhase = ''
            cp ${./my_tail.R} my_tail.R
      Rscript -e "
        source('libraries.R')
        mtcars_head <- read.csv('${mtcars_head}/mtcars_head')
                source('my_tail.R')
        mtcars_tail <- my_tail(mtcars_head)
        qs::qsave(mtcars_tail, 'mtcars_tail')"
    '';
  };

  mtcars_mpg = makeRDerivation {
    name = "mtcars_mpg";
    buildInputs = default2BuildInputs;
    configurePhase = default2ConfigurePhase;
    buildPhase = ''
      Rscript -e "
        source('libraries.R')
        mtcars_head <- read.csv('${mtcars_head}/mtcars_head')
        mtcars_tail <- qs::qread('${mtcars_tail}/mtcars_tail')
        mtcars_mpg <- full_join(mtcars_tail, mtcars_head)
        saveRDS(mtcars_mpg, 'mtcars_mpg')"
    '';
  };

  page = defaultPkgs.stdenv.mkDerivation {
    name = "page";
     src = defaultPkgs.lib.fileset.toSource {
      root = ./.;
      fileset = defaultPkgs.lib.fileset.unions [ ./page.qmd ./content.qmd ./images ];
    };
    buildInputs = quarto_envBuildInputs;
    configurePhase = quarto_envConfigurePhase;
    buildPhase = ''
      mkdir home
      export HOME=$PWD/home
      export RETICULATE_PYTHON=${defaultPkgs.python3}/bin/python
      substituteInPlace page.qmd --replace-fail 'rixpress::rxp_read("mtcars_head")' 'rixpress::rxp_read("${mtcars_head}")'
      substituteInPlace page.qmd --replace-fail 'rixpress::rxp_read("mtcars_tail")' 'rixpress::rxp_read("${mtcars_tail}")'
      substituteInPlace page.qmd --replace-fail 'rixpress::rxp_read("mtcars_mpg")' 'rixpress::rxp_read("${mtcars_mpg}")'
      quarto render page.qmd  --output-dir $out
    '';
  };

  # Generic default target that builds all derivations
  allDerivations = defaultPkgs.symlinkJoin {
    name = "all-derivations";
    paths = with builtins; attrValues { inherit mtcars mtcars_am mtcars_head mtcars_tail mtcars_mpg page; };
  };

in
{
  inherit mtcars mtcars_am mtcars_head mtcars_tail mtcars_mpg page;
  default = allDerivations;
}
