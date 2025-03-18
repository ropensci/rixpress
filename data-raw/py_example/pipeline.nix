let
  default = import ./default.nix;
  defaultPkgs = default.pkgs;
  defaultShell = default.shell;
  defaultBuildInputs = defaultShell.buildInputs;
  defaultConfigurePhase = ''
    cp ${./_rixpress/default_libraries.py} libraries.py
    mkdir -p $out
  '';
  
  # Function to create Python derivations
  makePyDerivation = { name, buildInputs, configurePhase, buildPhase, src ? null }:
    let
      pickleFile = "${name}.pkl";
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
    diabetes_raw = makePyDerivation {
    name = "diabetes_raw";
    buildInputs = defaultBuildInputs;
    configurePhase = defaultConfigurePhase;
    buildPhase = ''
      python -c "exec(open('libraries.py').read()); exec('diabetes_raw = sklearn.datasets.load_diabetes()'); import pickle; with open('diabetes_raw.pkl', 'wb') as f: pickle.dump(globals()['diabetes_raw'], f)"
    '';
  };

  test_data_head = makePyDerivation {
    name = "test_data_head";
    buildInputs = defaultBuildInputs;
    configurePhase = defaultConfigurePhase;
    buildPhase = ''
  import pickle
  with open('${diabetes_raw}/diabetes_raw.pkl', 'rb') as f:
      diabetes_raw = pickle.load(f)
      python -c "exec(open('libraries.py').read()); exec('test_data_head = pandas.DataFrame(diabetes.data, columns=diabetes_raw.feature_names)'); import pickle; with open('test_data_head.pkl', 'wb') as f: pickle.dump(globals()['test_data_head'], f)"
    '';
  };

  diabetes_head = makePyDerivation {
    name = "diabetes_head";
    buildInputs = defaultBuildInputs;
    configurePhase = defaultConfigurePhase;
    buildPhase = ''
      python -c "exec(open('libraries.py').read()); exec('diabetes_head = diabetes.head()'); import pickle; with open('diabetes_head.pkl', 'wb') as f: pickle.dump(globals()['diabetes_head'], f)"
    '';
  };

  # Generic default target that builds all derivations
  allDerivations = defaultPkgs.symlinkJoin {
    name = "all-derivations";
    paths = with builtins; attrValues { inherit diabetes_raw test_data_head diabetes_head; };
  };

in
{
  inherit diabetes_raw test_data_head diabetes_head;
  default = allDerivations;
}
