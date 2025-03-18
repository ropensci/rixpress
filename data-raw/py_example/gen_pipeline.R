library(rixpress)
library(igraph)

d0 <- rxp_py(diabetes_raw, 'sklearn.datasets.load_diabetes()')
d1 <- rxp_py(
  diabetes,
  'pandas.DataFrame(diabetes_raw.data, columns=diabetes_raw.feature_names)'
)
d2 <- rxp_py(diabetes_head, 'diabetes.head()')
d3 <- rxp_py(diabetes_tail, 'diabetes.tail()')
d4 <- rxp_py(
  concat_diabetes,
  'pandas.concat([diabetes_head, diabetes_tail], ignore_index=True)'
)

rxp_list <- list(d0, d1, d2, d3, d4)

rixpress(rxp_list, project_path = ".")

#plot_dag()

dag_obj <- plot_dag(return_igraph = TRUE)

dag_obj <- set_vertex_attr(dag_obj, "label", value = V(dag_obj)$name)

# Step 2: Delete the "name" attribute
dag_obj <- delete_vertex_attr(dag_obj, "name")

igraph::write_graph(dag_obj, file = "dag.dot", format = "dot")
