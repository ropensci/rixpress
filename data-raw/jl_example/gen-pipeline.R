library(rixpress)
library(igraph)

list(
  rxp_jl(d_size, '150'),

  rxp_jl(
    data,
    '0.1randn(d_size,d_size) + reshape(cholesky(gridlaplacian(d_size,d_size) + 0.003I) \ randn(d_size*d_size), d_size, d_size)',
    additional_files = "functions.jl"
  ),
  rxp_jl(
    laplace_df,
    'DataFrame(data, :auto)',
    serialize_function = 'arrow_write',
    additional_files = "functions.jl"
  )
) |>
  rixpress(build = FALSE)

#plot_dag()

#rxp_make()
