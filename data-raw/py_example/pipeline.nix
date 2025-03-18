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
    test_data = makePyDerivation {
    name = "test_data";
    buildInputs = defaultBuildInputs;
    configurePhase = defaultConfigurePhase;
    buildPhase = ''
      python -c "exec(open('libraries.py').read()); exec('test_data = pd.util.testing.makeDataFrame()'); import pickle; with open('test_data.pkl', 'wb') as f: pickle.dump(globals()['test_data'], f)"
    '';
  };

  test_data_head = makePyDerivation {
    name = "test_data_head";
    buildInputs = defaultBuildInputs;
    configurePhase = defaultConfigurePhase;
    buildPhase = ''
  import pickle
  with open('${test_data}/test_data.pkl', 'rb') as f:
      test_data = pickle.load(f)
      python -c "exec(open('libraries.py').read()); exec('test_data_head = test_data.head()'); import pickle; with open('test_data_head.pkl', 'wb') as f: pickle.dump(globals()['test_data_head'], f)"
    '';
  };

  # Generic default target that builds all derivations
  allDerivations = defaultPkgs.symlinkJoin {
    name = "all-derivations";
    paths = with builtins; attrValues { inherit test_data test_data_head; };
  };

in
{
  inherit test_data test_data_head;
  default = allDerivations;
}
