# rixpress 0.12.2 (2026-02-19)

- Quick fix: added maybe to Suggests

# rixpress 0.12.1 (2026-02-04)

## Improvements

- **Python Version Detection**: `rixpress` now correctly detects the Python version specified in `default.nix` (e.g., `python311Packages`, `python312Packages`) and uses the corresponding interpreter (e.g., `${defaultPkgs.python311}/bin/python`) instead of defaulting to `python3`. This prevents ABI version mismatch errors when using compiled Python packages like `numpy`.

# rixpress 0.12.0 (2026-01-31)

## New Features

- **Chronicler integration**: Added support for detecting `{chronicler}` package
  `Nothing` values in pipeline outputs. When using `chronicler`'s `record()`
  decorated functions, errors and warnings are captured as `Nothing` values
  instead of failing the build. This can mask pipeline failures.

- **`rxp_check_chronicles()`**: New function to scan all pipeline outputs for
  chronicle objects and report their status:
  - ✓ Success: `Just` value, no warnings or errors
  - ⚠ Warning: `Just` value, but warnings were captured
  - ✗ Nothing: Failed computation, errors captured

- **Enhanced `rxp_read()` and `rxp_load()`**: Now automatically warn when
  reading chronicle objects that contain `Nothing` values, and inform about
  chronicles with captured warnings.

## Internal Changes

- Added `chronicler` as an optional dependency (Suggests).
- New internal helper functions for chronicle state detection.

# rixpress 0.11.2 (2026-01-28)

## Changes

- Updated `rxp_visnetwork()` styling to use a dual-encoding approach (similar to `rxp_ggdag()`) when `color_by = "pipeline"`. Now, node interiors are colored by derivation type (e.g., R, Python) while node borders are colored by pipeline group.
- Spellchecked `vignettes/sub-pipelines.Rmd` to British English.
# rixpress 0.11.1 (2026-01-25)

## Bug Fixes

- Fixed parsing of default.nix files when Python packages are installed from git.
  Previously, `rixpress` failed to parse the `pyconf` block correctly when it contained
  the `} ++ [ ... ];` syntax used by `rix` for git packages.



# rixpress 0.11.0 (2025-01-24)

## New Features

- **Sub-pipelines with `rxp_pipeline()`**: New function to organize derivations
  into named logical groups (e.g., "ETL", "Model", "Report"). Pipeline groups
  are visually distinguished in DAG visualizations with custom colors.

- **Pipeline-based DAG coloring**: `rxp_visnetwork()` and `rxp_ggdag()` now
  support a `color_by` parameter. When set to "pipeline" (the default when
  pipelines are defined), nodes are colored by their pipeline group rather
  than derivation type.

- **Master Script workflow**: Enables organizing large projects across multiple
  R scripts, then combining them in a master script using `rxp_pipeline()`.

## Internal Changes

- `rxp_populate()` now flattens `rxp_pipeline` objects while preserving metadata.
- `dag.json` schema extended with `pipeline_group` and `pipeline_color` fields.
- `get_nodes_edges()` extracts pipeline metadata for visualization.

## Bug Fixes

- Relative paths for `nix_env` (e.g., `"../../default.nix"`) now work correctly.
  Previously, path characters were converted to underscores, causing invalid
  Nix variable names like `______defaultBuildInputs`. Now only the filename
  is used, producing valid identifiers like `defaultBuildInputs`.

# rixpress 0.10.2 (2025-01-24)

Fixed bug: relative paths for `nix_env` (e.g., `"../../default.nix"`) now work
correctly. Previously, path characters were converted to underscores, causing
invalid Nix variable names like `______defaultBuildInputs`. Now only the
filename is used, producing valid identifiers like `defaultBuildInputs`.

# rixpress 0.10.1 (2025-10-07)

Fixed bug: functions inside of arbitrary folder were not being
imported into the build sandbox correctly.

# rixpress 0.10.0 (2025-10-07)

First CRAN release!

- Better readme.

# rixpress 0.3.0 (2025-09-16)

rixpress in now an RopenSci package!

# rixpress 0.2.0 (2025-05-12)

Submission for RopenSci review.

- `rxp_rmd()`: build RMD documents.
- `rxp_list_logs()`: list the logs of the builds, and possible to read
  artifacts of previous builds with `rxp_read()` (or load with `rxp_load()` as well.
- DAG of pipeline can be visualised with `{visNetwork}` or with `{ggdag}`.

# rixpress 0.1.0 (2025-04-14)

First release (only on GitHub).

## New features

- Possibility to define pipelines with R, Python or Quarto outputs.
  Data transfer between R and Python is made using `{reticulate}`.
- Basic plotting of DAG of pipeline.
- Demos available at: https://github.com/b-rodrigues/rixpress_demos


