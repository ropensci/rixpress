library(rixpress)
library(igraph)

d0 <- rxp_py(test_data, 'pd.util.testing.makeDataFrame()')
d1 <- rxp_py(test_data_head, 'test_data.head()')

rxp_list <- list(d0, d1)

rixpress(rxp_list, project_path = ".")

plot_dag()

dag_obj <- plot_dag(return_igraph = TRUE)

dag_obj <- set_vertex_attr(dag_obj, "label", value = V(dag_obj)$name)

# Step 2: Delete the "name" attribute
dag_obj <- delete_vertex_attr(dag_obj, "name")

#igraph::write_graph(dag_obj, file = "dag.dot", format = "dot")
