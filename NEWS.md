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
