library(rixpress)
library(igraph)

list(
  rxp_r_file(
    name = NULL,
    path = NULL,
    read_function = \(x) read.csv(file = x, sep = ",")
  ),
  rxp_r(
    name = NULL,
    expr = NULL
  )
) |>
  rxp_populate(build = FALSE)
