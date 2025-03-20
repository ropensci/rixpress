let
  py_env = import ./py-env.nix;
  py_envPkgs = py_env.pkgs;
  py_envShell = py_env.shell;
  py_envBuildInputs = py_envShell.buildInputs;
  py_envConfigurePhase = ''
    cp ${./_rixpress/py_env_libraries.py} libraries.py
    mkdir -p $out
  '';
default = import ./default.nix;
  defaultPkgs = default.pkgs;
  defaultShell = default.shell;
  defaultBuildInputs = defaultShell.buildInputs;
  defaultConfigurePhase = ''
    cp ${./_rixpress/default_libraries.R} libraries.R
    mkdir -p $out
  '';
quarto_env = import ./quarto-env.nix;
  quarto_envPkgs = quarto_env.pkgs;
  quarto_envShell = quarto_env.shell;
  quarto_envBuildInputs = quarto_envShell.buildInputs;
  quarto_envConfigurePhase = ''
    cp ${./_rixpress/quarto_env_libraries.R} libraries.R
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
  # Function to create Python derivations
  makePyDerivation = { name, buildInputs, configurePhase, buildPhase, src ? null }:
    let
      pickleFile = "${name}.pickle";
    in
      defaultPkgs.stdenv.mkDerivation {
        inherit name src;
        dontUnpack = true;
        buildInputs = buildInputs;
        inherit configurePhase buildPhase;
        installPhase = ''
          cp ${pickleFile} $out
        '';
      };

  # Define all derivations
    mtcars_pl = makePyDerivation {
    name = "mtcars_pl";
    src = ./data/mtcars.csv;
    buildInputs = py_envBuildInputs;
    configurePhase = py_envConfigurePhase;
    buildPhase = ''
      cp $src input_file
python -c "
exec(open('libraries.py').read())
file_path = 'input_file'
data = eval('lambda x: polars.read_csv(x, separator=\'|\')')(file_path)
with open('mtcars_pl.pickle', 'wb') as f:
    pickle.dump(data, f)
"

    '';
  };

  mtcars_pl_am = makePyDerivation {
    name = "mtcars_pl_am";
    buildInputs = py_envBuildInputs;
    configurePhase = py_envConfigurePhase;
    buildPhase = ''
      python -c "
exec(open('libraries.py').read())
with open('${mtcars_pl}/mtcars_pl.pickle', 'rb') as f: mtcars_pl = pickle.load(f)
exec('mtcars_pl_am = mtcars_pl.filter(polars.col(\'am\') == 1).to_pandas()')
with open('mtcars_pl_am.pickle', 'wb') as f: pickle.dump(globals()['mtcars_pl_am'], f)"
    '';
  };

  mtcars_am = makeRDerivation {
    name = "mtcars_am";
    buildInputs = defaultBuildInputs;
    configurePhase = defaultConfigurePhase;
    buildPhase = ''
      export RETICULATE_PYTHON='${defaultPkgs.python3}/bin/python'
       Rscript -e "
         source('libraries.R')
         mtcars_am <- reticulate::py_load_object('${mtcars_pl_am}/mtcars_pl_am.pickle', pickle = 'pickle', convert = TRUE)
         saveRDS(mtcars_am, 'mtcars_am.rds')"
    '';
  };

  mtcars_head = makeRDerivation {
    name = "mtcars_head";
    buildInputs = defaultBuildInputs;
    configurePhase = defaultConfigurePhase;
    buildPhase = ''
      Rscript -e "
        source('libraries.R')
        mtcars_am <- readRDS('${mtcars_am}/mtcars_am.rds')
        mtcars_head <- my_head(mtcars_am)
        saveRDS(mtcars_head, 'mtcars_head.rds')"
    '';
  };

  mtcars_tail = makeRDerivation {
    name = "mtcars_tail";
    buildInputs = defaultBuildInputs;
    configurePhase = defaultConfigurePhase;
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
    buildInputs = defaultBuildInputs;
    configurePhase = defaultConfigurePhase;
    buildPhase = ''
      Rscript -e "
        source('libraries.R')
        mtcars_tail <- readRDS('${mtcars_tail}/mtcars_tail.rds')
        mtcars_mpg <- dplyr::select(mtcars_tail, mpg)
        saveRDS(mtcars_mpg, 'mtcars_mpg.rds')"
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
  substituteInPlace page.qmd --replace-fail 'rxp_read("mtcars_head")' 'rxp_read("${mtcars_head}/mtcars_head.rds")'
  substituteInPlace page.qmd --replace-fail 'rxp_read("mtcars_tail")' 'rxp_read("${mtcars_tail}/mtcars_tail.rds")'
  substituteInPlace page.qmd --replace-fail 'rxp_read("mtcars_mpg")' 'rxp_read("${mtcars_mpg}/mtcars_mpg.rds")'
  quarto render page.qmd --output-dir $out
    '';
  };

  # Generic default target that builds all derivations
  allDerivations = defaultPkgs.symlinkJoin {
    name = "all-derivations";
    paths = with builtins; attrValues { inherit mtcars_pl mtcars_pl_am mtcars_am mtcars_head mtcars_tail mtcars_mpg page; };
  };

in
{
  inherit mtcars_pl mtcars_pl_am mtcars_am mtcars_head mtcars_tail mtcars_mpg page;
  default = allDerivations;
}
