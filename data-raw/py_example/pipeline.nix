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
      pickleFile = "${name}";
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
      python -c "
exec(open('libraries.py').read())
exec('diabetes_raw = sklearn.datasets.load_diabetes()')
with open('diabetes_raw', 'wb') as f: pickle.dump(globals()['diabetes_raw'], f)"
    '';
  };

  diabetes = makePyDerivation {
    name = "diabetes";
    buildInputs = defaultBuildInputs;
    configurePhase = defaultConfigurePhase;
    buildPhase = ''
      python -c "
exec(open('libraries.py').read())
with open('${diabetes_raw}/diabetes_raw', 'rb') as f: diabetes_raw = pickle.load(f)
exec('diabetes = pandas.DataFrame(diabetes_raw.data, columns=diabetes_raw.feature_names)')
with open('diabetes', 'wb') as f: pickle.dump(globals()['diabetes'], f)"
    '';
  };

  diabetes_head = makePyDerivation {
    name = "diabetes_head";
    buildInputs = defaultBuildInputs;
    configurePhase = defaultConfigurePhase;
    buildPhase = ''
      python -c "
exec(open('libraries.py').read())
with open('${diabetes}/diabetes', 'rb') as f: diabetes = pickle.load(f)
exec('diabetes_head = my_head(diabetes)')
with open('diabetes_head', 'wb') as f: pickle.dump(globals()['diabetes_head'], f)"
    '';
  };

  diabetes_tail = makePyDerivation {
    name = "diabetes_tail";
    buildInputs = defaultBuildInputs;
    configurePhase = defaultConfigurePhase;
    buildPhase = ''
      python -c "
exec(open('libraries.py').read())
with open('${diabetes}/diabetes', 'rb') as f: diabetes = pickle.load(f)
exec('diabetes_tail = diabetes.tail()')
with open('diabetes_tail', 'wb') as f: pickle.dump(globals()['diabetes_tail'], f)"
    '';
  };

  concat_diabetes = makePyDerivation {
    name = "concat_diabetes";
    buildInputs = defaultBuildInputs;
    configurePhase = defaultConfigurePhase;
    buildPhase = ''
      python -c "
exec(open('libraries.py').read())
with open('${diabetes_head}/diabetes_head', 'rb') as f: diabetes_head = pickle.load(f)
with open('${diabetes_tail}/diabetes_tail', 'rb') as f: diabetes_tail = pickle.load(f)
exec('concat_diabetes = pandas.concat([diabetes_head, diabetes_tail], ignore_index=True)')
with open('concat_diabetes', 'wb') as f: pickle.dump(globals()['concat_diabetes'], f)"
    '';
  };

  # Generic default target that builds all derivations
  allDerivations = defaultPkgs.symlinkJoin {
    name = "all-derivations";
    paths = with builtins; attrValues { inherit diabetes_raw diabetes diabetes_head diabetes_tail concat_diabetes; };
  };

in
{
  inherit diabetes_raw diabetes diabetes_head diabetes_tail concat_diabetes;
  default = allDerivations;
}
