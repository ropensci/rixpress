test_that("get_nodes_edges processes dag.json correctly", {
  testthat::skip_on_cran()
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
      group = c("rxp_r", "rxp_r", "rxp_r", "rxp_r", "rxp_r", "rxp_qmd"),
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

test_that("rxp_dag_for_ci generates correct .dot file", {
  testthat::skip_on_cran()
  test_dag_path <- "test-data/dag.json"

  nodes_and_edges <- get_nodes_edges(path_dag = test_dag_path)

  snapshot_rxp_dag_for_ci <- function(nodes_and_edges) {
    tfile <- tempfile(pattern = "dag_ci", fileext = ".dot")

    rxp_dag_for_ci(nodes_and_edges = nodes_and_edges, output_file = tfile)

    tfile
  }

  expect_snapshot_file(
    path = snapshot_rxp_dag_for_ci(nodes_and_edges),
    name = "rxp_dag_for_ci.dot"
  )
})

test_that("get_nodes_edges errors if dag.json is missing", {
  # Use a path that is highly unlikely to exist
  non_existent_path <- "a/very/unlikely/path/to/dag.json"

  # Expect the specific error message
  expect_error(
    get_nodes_edges(path_dag = non_existent_path),
    regexp = "dag\\.json missing! Did you run 'rxp_populate\\(\\)'\\?",
    fixed = FALSE # Use regexp matching because of special characters
  )
})

test_that("unnest_all_columns correctly unnests data frames", {
  # Create a test data frame with a list column containing data frames
  test_df <- data.frame(
    id = 1:2,
    stringsAsFactors = FALSE
  )
  test_df$nested <- list(
    data.frame(
      colA = c("a", "b"),
      colB = c("x", "y"),
      stringsAsFactors = FALSE
    ),
    data.frame(colA = c("c", "d"), colB = c("z", "w"), stringsAsFactors = FALSE)
  )

  result <- unnest_all_columns(test_df)

  expected <- data.frame(
    id = c(1, 2, 1, 2),
    colA = c("a", "b", "c", "d"),
    colB = c("x", "y", "z", "w"),
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected)
})
