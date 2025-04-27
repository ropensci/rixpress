test_that("get_nodes_edges processes dag.json correctly", {
  test_dag_path <- "test-data/dag.json"

  actual_output <- get_nodes_edges(path_dag = test_dag_path)

  expected_output <- list(
    nodes = data.frame(
      id = c(
        "mtcars",
        "mtcars_am",
        "mtcars_head",
        "mtcars_tail",
        "mtcars_mpg",
        "page"
      ),
      label = c(
        "mtcars",
        "mtcars_am",
        "mtcars_head",
        "mtcars_tail",
        "mtcars_mpg",
        "page"
      ),
      group = c("rxp_r", "rxp_r", "rxp_r", "rxp_r", "rxp_r", "rxp_quarto"),
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      from = c(
        "mtcars",
        "mtcars_am",
        "mtcars_head",
        "mtcars_tail",
        "mtcars_head",
        "mtcars_tail",
        "mtcars_mpg"
      ),
      to = c(
        "mtcars_am",
        "mtcars_head",
        "mtcars_tail",
        "mtcars_mpg",
        "page",
        "page",
        "page"
      ),
      arrows = rep("to", 7),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(actual_output, expected_output)
})

test_that("dag_for_ci generates correct .dot file", {
  test_dag_path <- "test-data/dag.json"

  nodes_and_edges <- get_nodes_edges(path_dag = test_dag_path)

  snapshot_dag_for_ci <- function(nodes_and_edges) {
    tfile <- tempfile(pattern = "dag_ci", fileext = ".dot")

    dag_for_ci(nodes_and_edges = nodes_and_edges, output_file = tfile)

    tfile
  }

  expect_snapshot_file(
    path = snapshot_dag_for_ci(nodes_and_edges),
    name = "dag_for_ci.dot"
  )
})

test_that("get_nodes_edges errors if dag.json is missing", {
  # Use a path that is highly unlikely to exist
  non_existent_path <- "a/very/unlikely/path/to/dag.json"

  # Expect the specific error message
  expect_error(
    get_nodes_edges(path_dag = non_existent_path),
    regexp = "dag\\.json missing! Did you run 'rixpress\\(\\)'\\?",
    fixed = FALSE # Use regexp matching because of special characters
  )
})
