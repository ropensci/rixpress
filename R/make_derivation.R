#' mk_r Creates a Nix expression running an R function
#' @param output Character, a variable name to save the
#'   output of the function expression
#' @param f A function expression, e.g., sqrt(2)
#' @param ide Character, the ide to use.
#' @noRd
mk_r <- function(output, f) {
  f_name <- as.character(substitute(f))[[1]]

  sprintf(
    "
let
  shell = import ./default.nix { inherit pkgs; };
in
r_%s = pkgs.stdenv.mkDerivation {
  name = \"%s\";
  buildInputs = shell.buildInputs;  # Use the build inputs from the shell
  dontUnpack = true;

  configurePhase = ''
    mkdir -p $out
  '';

  buildPhase = ''
    Rscript -e \" library(XXX); %s <- f;saveRDS(%s, $out) \"
  '';

  installPhase = ''
  cp %s $out/
       '';
};
",
    f_name,
    f_name,
    output,
    output,
    output
  )
}
