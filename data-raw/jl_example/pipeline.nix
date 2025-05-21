let
  default = import ./default.nix;
  defaultPkgs = default.pkgs;
  defaultShell = default.shell;
  defaultBuildInputs = defaultShell.buildInputs;
  defaultConfigurePhase = ''
    cp ${./_rixpress/default_libraries.jl} libraries.jl
    cp ${./_rixpress/default_libraries.R} libraries.R
    mkdir -p $out
  '';
  
  # Function to create Julia derivations
  makeJlDerivation = { name, buildInputs, configurePhase, buildPhase, src ? null }:
    defaultPkgs.stdenv.mkDerivation {
      inherit name src;
      dontUnpack = true;
      buildInputs = buildInputs;
      inherit configurePhase buildPhase;
      installPhase = ''
        cp ${name} $out/
      '';
    };

  # Define all derivations
    d_size = makeJlDerivation {
    name = "d_size";
    buildInputs = defaultBuildInputs;
    configurePhase = defaultConfigurePhase;
    buildPhase = ''
      julia -e "
if isfile(\"libraries.jl\"); include(\"libraries.jl\"); end; 
d_size = 150; 
using Serialization; io = open("d_size", "w"); serialize(io, d_size); close(io)
"
    '';
  };

  data = makeJlDerivation {
    name = "data";
    buildInputs = defaultBuildInputs;
    configurePhase = defaultConfigurePhase;
    buildPhase = ''
      julia -e "
if isfile(\"libraries.jl\"); include(\"libraries.jl\"); end; 
d_size = Serialization.deserialize(\"${d_size}/d_size\")
data = 0.1randn(d_size,d_size) + reshape(cholesky(gridlaplacian(d_size,d_size) + 0.003I)  randn(d_size*d_size), d_size, d_size); 
using Serialization; io = open("data", "w"); serialize(io, data); close(io)
"
    '';
  };

  laplace_df = makeJlDerivation {
    name = "laplace_df";
    buildInputs = defaultBuildInputs;
    configurePhase = defaultConfigurePhase;
    buildPhase = ''
      julia -e "
if isfile(\"libraries.jl\"); include(\"libraries.jl\"); end; 
data = Serialization.deserialize(\"${data}/data\")
laplace_df = DataFrame(data, :auto); 
arrow_write(laplace_df, "laplace_df")
"
    '';
  };

  # Generic default target that builds all derivations
  allDerivations = defaultPkgs.symlinkJoin {
    name = "all-derivations";
    paths = with builtins; attrValues { inherit d_size data laplace_df; };
  };

in
{
  inherit d_size data laplace_df;
  default = allDerivations;
}
