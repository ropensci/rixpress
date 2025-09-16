# This script defines the default environment the pipeline runs in.
# Add the required packages to execute the code necessary for each derivation.
# If you want to create visual representations of the pipeline, consider adding
# `{visNetwork}` and `{ggdag}` to the list of R packages.
library(rix)

# Define execution environment
rix(
  date = NULL,
  r_pkgs = NULL,
  py_conf = NULL,
  git_pkgs = list(
    "package_name" = "rixpress",
    "repo_url" = "https://github.com/ropensci/rixpress",
    "commit" = "HEAD",
  ),
  ide = "none",
  project_path = "."
)
