rxp_make <- function() {
  system("nix-build -o_rixpress/result pipeline.nix")
}
