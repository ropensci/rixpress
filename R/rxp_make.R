#' Build pipeline using Nix
#'
#' Runs `nix-build` with a quiet flag, outputting to `_rixpress/result`.
#' @export
rxp_make <- function() {
  system("nix-build --quiet -o_rixpress/result pipeline.nix")
}
