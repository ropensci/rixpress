test_that("rxp_pipeline creates valid pipeline object", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("rix")

  # Create derivations
  d1 <- rxp_r(test1, 1 + 1)
  d2 <- rxp_r(test2, 2 + 2)

  # Create a pipeline
  pipe <- rxp_pipeline("TestPipe", list(d1, d2), color = "#FF0000")

  # Check structure
  expect_s3_class(pipe, "rxp_pipeline")
  expect_equal(pipe$name, "TestPipe")
  expect_equal(pipe$color, "#FF0000")
  expect_length(pipe$derivs, 2)

  # Check that metadata is attached to derivations
  expect_equal(pipe$derivs[[1]]$pipeline_group, "TestPipe")
  expect_equal(pipe$derivs[[1]]$pipeline_color, "#FF0000")
  expect_equal(pipe$derivs[[2]]$pipeline_group, "TestPipe")
  expect_equal(pipe$derivs[[2]]$pipeline_color, "#FF0000")
})

test_that("rxp_pipeline validates inputs", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("rix")

  d1 <- rxp_r(test1, 1 + 1)

  # Empty name should error
  expect_error(rxp_pipeline("", list(d1)), "non-empty character string")

  # Single derivation (not in list) should error with helpful message
  expect_error(
    rxp_pipeline("Pipe", d1),
    "'path' must be a list of derivation objects or a file path"
  )

  # Non-derivation in list should error
  expect_error(
    rxp_pipeline("Pipe", list(d1, "not a deriv")),
    "not an rxp_derivation object"
  )
})

test_that("rxp_pipeline sources from file", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("rix")

  # Create a temporary R script defining a pipeline
  temp_script <- tempfile(fileext = ".R")
  writeLines(
    'library(rixpress)
     list(
       rxp_r(name = d1, expr = 1 + 1),
       rxp_r(name = d2, expr = 2 + 2)
     )',
    temp_script
  )
  on.exit(unlink(temp_script))

  # Create pipeline from file
  pipe <- rxp_pipeline("FilePipe", temp_script, color = "green")

  expect_s3_class(pipe, "rxp_pipeline")
  expect_equal(pipe$name, "FilePipe")
  expect_length(pipe$derivs, 2)
  expect_equal(pipe$derivs[[1]]$name, "d1")
})

test_that("flatten_derivations works correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("rix")

  # Create derivations
  d1 <- rxp_r(etl1, 1)
  d2 <- rxp_r(etl2, 2)
  d3 <- rxp_r(model1, 3)
  d4 <- rxp_r(model2, 4)

  # Create pipelines
  pipe_etl <- rxp_pipeline("ETL", list(d1, d2), color = "orange")
  pipe_model <- rxp_pipeline("Model", list(d3, d4), color = "blue")

  # Flatten
  result <- flatten_derivations(list(pipe_etl, pipe_model))

  # Check result
  expect_length(result, 4)
  expect_equal(result[[1]]$name, "etl1")
  expect_equal(result[[1]]$pipeline_group, "ETL")
  expect_equal(result[[1]]$pipeline_color, "orange")
  expect_equal(result[[3]]$name, "model1")
  expect_equal(result[[3]]$pipeline_group, "Model")
  expect_equal(result[[3]]$pipeline_color, "blue")
})

test_that("flatten_derivations handles flat lists with defaults", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("rix")

  # Create plain derivations (no pipeline)
  d1 <- rxp_r(plain1, 1)
  d2 <- rxp_r(plain2, 2)

  # Flatten
  result <- flatten_derivations(list(d1, d2))

  # Check that defaults are applied
  expect_length(result, 2)
  expect_equal(result[[1]]$pipeline_group, "default")
  expect_null(result[[1]]$pipeline_color)
})

test_that("rxp_populate handles rxp_pipeline objects", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("rix")

  # Create temporary directory
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  rix::rix(
    date = "2025-04-11",
    r_pkgs = "dplyr",
    git_pkgs = list(
      package_name = "rixpress",
      repo_url = "https://github.com/ropensci/rixpress",
      commit = "HEAD"
    ),
    ide = "none",
    project_path = temp_dir,
    overwrite = TRUE
  )

  setwd(temp_dir)

  # Create derivations and pipelines
  d1 <- rxp_r(etl_clean, 1 + 1)
  d2 <- rxp_r(model_train, etl_clean + 1)

  pipe_etl <- rxp_pipeline("Data Prep", list(d1), color = "#E69F00")
  pipe_model <- rxp_pipeline("Modeling", list(d2), color = "#56B4E9")

  # Populate
  rxp_populate(list(pipe_etl, pipe_model), project_path = ".", build = FALSE)

  # Check that dag.json contains pipeline metadata
  dag_json <- jsonlite::fromJSON("_rixpress/dag.json")

  expect_true("pipeline_group" %in% names(dag_json$derivations))
  expect_true("pipeline_color" %in% names(dag_json$derivations))

  # Check specific values (handle both list and vector formats from jsonlite)
  etl_idx <- which(dag_json$derivations$deriv_name == "etl_clean")
  model_idx <- which(dag_json$derivations$deriv_name == "model_train")

  # Use unlist to handle potential list format
  etl_group <- unlist(dag_json$derivations$pipeline_group[etl_idx])
  etl_color <- unlist(dag_json$derivations$pipeline_color[etl_idx])
  model_group <- unlist(dag_json$derivations$pipeline_group[model_idx])
  model_color <- unlist(dag_json$derivations$pipeline_color[model_idx])

  expect_equal(etl_group, "Data Prep")
  expect_equal(etl_color, "#E69F00")
  expect_equal(model_group, "Modeling")
  expect_equal(model_color, "#56B4E9")
})

test_that("get_nodes_edges reads pipeline metadata from dag.json", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("rix")

  # Create a temporary dag.json with pipeline metadata
  temp_dir <- tempfile()
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  dag_content <- list(
    derivations = data.frame(
      deriv_name = c("node1", "node2"),
      depends = I(list(character(0), "node1")),
      decoder = c("rds", "rds"),
      type = c("rxp_r", "rxp_r"),
      noop_build = c(FALSE, FALSE),
      pipeline_group = c("Pipeline A", "Pipeline B"),
      pipeline_color = c("#FF0000", "#00FF00"),
      stringsAsFactors = FALSE
    )
  )

  dag_path <- file.path(temp_dir, "dag.json")
  jsonlite::write_json(dag_content, dag_path, auto_unbox = TRUE)

  # Read with get_nodes_edges
  result <- get_nodes_edges(path_dag = dag_path)

  # Check that pipeline metadata is present
  expect_true("pipeline_group" %in% names(result$nodes))
  expect_true("pipeline_color" %in% names(result$nodes))

  expect_equal(result$nodes$pipeline_group[1], "Pipeline A")
  expect_equal(result$nodes$pipeline_group[2], "Pipeline B")
  expect_equal(result$nodes$pipeline_color[1], "#FF0000")
  expect_equal(result$nodes$pipeline_color[2], "#00FF00")
})

test_that("print.rxp_pipeline works", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("rix")

  d1 <- rxp_r(test1, 1)
  pipe <- rxp_pipeline("MyPipe", list(d1), color = "red")

  # Capture print output
  output <- capture.output(print(pipe))

  expect_true(any(grepl("MyPipe", output)))
  expect_true(any(grepl("red", output)))
  expect_true(any(grepl("test1", output)))
})
