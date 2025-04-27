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
