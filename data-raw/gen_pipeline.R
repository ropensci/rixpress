library(rixpress)

d0 <- rxp_file(
  mtcars,
  'mtcars.csv',
  \(x) (read.csv(file = x, sep = "|")),
  nix_env = "default.nix"
)
d1 <- rxp_r(mtcars_am, filter(mtcars, am == 1), nix_env = "default2.nix")
d2 <- rxp_r(mtcars_head, head(mtcars_am), nix_env = "default.nix")
d3 <- rxp_r(mtcars_tail, tail(mtcars_head), nix_env = "default.nix")
d4 <- rxp_r(mtcars_mpg, select(mtcars_tail, mpg), nix_env = "default2.nix")
doc <- rxp_quarto(
  page,
  "page.qmd",
  additional_files = c("content.qmd", "images"),
  nix_env = "quarto-env.nix"
)

rxp_list <- list(d0, d1, d2, d3, d4, doc)

rixpress(rxp_list, project_path = ".")

png("dag.png")
dag_obj <- plot_dag(return_igraph = FALSE)
dev.off()

igraph::write_graph(dag_obj, file = "dag.dot", format = "dot")
