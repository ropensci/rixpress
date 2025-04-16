library(rix)

# Define execution environment
rix(
  date = NULL,
  r_pkgs = NULL,
  git_pkgs = list(
    package_name = "rixpress"
    repo_url = "https://github.com/b-rodrigues/rixpress"
    commit = "HEAD"
  )
  ide = "none",
  project_path = ".",
  overwrite = TRUE
)
