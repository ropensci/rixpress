library(rixpress)
library(igraph)

d0 <- rxp_py(diabetes_raw, 'sklearn.datasets.load_diabetes()')
d1 <- rxp_py(
  diabetes,
  'pandas.DataFrame(diabetes_raw.data, columns=diabetes_raw.feature_names)'
)
d2 <- rxp_py(
  diabetes_head,
  'my_head(diabetes)',
  user_functions = "functions.py"
)
d3 <- rxp_py(diabetes_tail, 'diabetes.tail()')
d4 <- rxp_py(
  concat_diabetes,
  'pandas.concat([diabetes_head, diabetes_tail], ignore_index=True)'
)

rxp_list <- list(d0, d1, d2, d3, d4)

rxp_populate(rxp_list, project_path = ".", build = TRUE)
