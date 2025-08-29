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
        mtcars_am <- filter(mtcars, am == 1)
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
        mtcars_head <- my_head(mtcars_am, 10)
        saveRDS(mtcars_head, 'mtcars_head')"
    '';
  };

  mtcars_tail = defaultPkgs.runCommand "mtcars_tail" {} "
    mkdir -p $out
    echo 'Build skipped for mtcars_tail' > $out/NOOPBUILD
  ";

  mtcars_mpg = defaultPkgs.runCommand "mtcars_mpg" {} "
    mkdir -p $out
    echo 'Build skipped for mtcars_mpg (cascading no-op)' > $out/NOOPBUILD
  ";

  page = defaultPkgs.runCommand "page" {} "
    mkdir -p $out
    echo 'Build skipped for page (cascading no-op)' > $out/NOOPBUILD
  ";

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
