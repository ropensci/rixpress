test_that("get_nodes_edges processes dag.json correctly", {
  # Path relative to the tests/testthat directory
  test_dag_path <- "test-data/dag.json"

  # Run the function
  actual_output <- get_nodes_edges(path_dag = test_dag_path)

  # Define the expected output structure
  expected_output <- list(
    nodes = data.frame(
      id = c("mtcars", "mtcars_am", "mtcars_head", "mtcars_tail", "mtcars_mpg", "page"),
      label = c("mtcars", "mtcars_am", "mtcars_head", "mtcars_tail", "mtcars_mpg", "page"),
      group = c("rxp_r", "rxp_r", "rxp_r", "rxp_r", "rxp_r", "rxp_quarto"),
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      from = c("mtcars", "mtcars_am", "mtcars_head", "mtcars_tail", "mtcars_head", "mtcars_tail", "mtcars_mpg"),
      to = c("mtcars_am", "mtcars_head", "mtcars_tail", "mtcars_mpg", "page", "page", "page"),
      arrows = rep("to", 7),
      stringsAsFactors = FALSE
    )
  )

  # Compare the actual output to the expected output
  expect_equal(actual_output, expected_output)
})

test_that("dag_for_ci generates correct .dot file", {
  # Path relative to the tests/testthat directory
  test_dag_path <- "test-data/dag.json"

  # Get the input data
  nodes_and_edges <- get_nodes_edges(path_dag = test_dag_path)

  # Define a function to generate the file and return its path
  # This follows the pattern used in test-gen-pipeline.R
  snapshot_dag_for_ci <- function(nodes_and_edges) {
    # Create a temporary file path for the .dot output
    # testthat handles cleanup of temp files created this way
    tfile <- tempfile(pattern = "dag_ci", fileext = ".dot")

    # Run the function to generate the .dot file
    dag_for_ci(nodes_and_edges = nodes_and_edges, output_file = tfile)

    # Return the path to the generated file
    return(tfile)
  }

  # Use expect_snapshot_file to compare the generated file content
  expect_snapshot_file(
    path = snapshot_dag_for_ci(nodes_and_edges),
    name = "dag_for_ci.dot"
  )
})
