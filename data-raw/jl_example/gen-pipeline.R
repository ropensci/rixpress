library(rixpress)
library(igraph)

list(
  rxp_jl(d_size, '150'),

rxp_jl(
  data,
  "0.1randn(d_size,d_size) + reshape( \
     cholesky(gridlaplacian(d_size,d_size) + 0.003I) \\ randn(d_size*d_size), \
     d_size, \
     d_size \
   )",
  additional_files = "functions.jl"
),
  rxp_jl(
    laplace_df,
    'DataFrame(data, :auto)',
    serialize_function = 'arrow_write',
    additional_files = "functions.jl"
  ),
rxp_r(
  laplace_long_df,
  prepare_data(laplace_df),
  unserialize_function = 'read_ipc_file',
  additional_files = "functions.R"
),
rxp_r(
  gg,
  make_gg(laplace_long_df)
)
) |>
  rixpress(build = TRUE)

#plot_dag()

#rxp_make()
