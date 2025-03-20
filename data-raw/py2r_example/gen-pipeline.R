library(rixpress)

d0 <- rxp_py_file(
  name = mtcars_pl,
  path = 'data/mtcars.csv',
  read_function = "lambda x: polars.read_csv(x, separator='|')",
  nix_env = "py-env.nix"
)

d1 <- rxp_py(
  # reticulate doesn't support polars DFs yet, so need to convert
  # first to pandas DF
  name = mtcars_pl_am,
  py_expr = "mtcars_pl.filter(polars.col('am') == 1).to_pandas()",
  nix_env = "py-env.nix"
)

d2 <- rxp_py2r(
  name = mtcars_am,
  expr = mtcars_pl_am
)

d3 <- rxp_r(
  name = mtcars_head,
  expr = my_head(mtcars_am),
  additional_files = "functions.R"
)

d4 <- rxp_r(
  name = mtcars_tail,
  expr = tail(mtcars_head)
)

d5 <- rxp_r(
  name = mtcars_mpg,
  expr = dplyr::select(mtcars_tail, mpg)
)

doc <- rxp_quarto(
  name = page,
  qmd_file = "page.qmd",
  additional_files = c("content.qmd", "images"),
  nix_env = "quarto-env.nix"
)

rxp_list <- list(d0, d1, d2, d3, d4, d5, doc)

rixpress(rxp_list, project_path = ".")

plot_dag()
