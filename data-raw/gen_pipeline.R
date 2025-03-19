library(rixpress)
library(igraph)

d0 <- rxp_r_file(
  mtcars,
  'mtcars.csv',
  \(x) (read.csv(file = x, sep = "|")),
  nix_env = "default.nix"
)
d1 <- rxp_r(mtcars_am, filter(mtcars, am == 1), nix_env = "default2.nix")

d2 <- rxp_r(
  mtcars_head,
  my_head(mtcars_am),
  additional_files = "my_head.R",
  nix_env = "default.nix"
)

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

dag_obj <- plot_dag(return_igraph = TRUE)

dag_obj <- set_vertex_attr(dag_obj, "label", value = V(dag_obj)$name)

# Step 2: Delete the "name" attribute
dag_obj <- delete_vertex_attr(dag_obj, "name")

igraph::write_graph(dag_obj, file = "dag.dot", format = "dot")
