test_that("get_concept_column returns correct column names", {
  expect_equal(get_concept_column("condition_occurrence"), "condition_concept_id")
  expect_equal(get_concept_column("procedure_occurrence"), "procedure_concept_id")
  expect_equal(get_concept_column("drug_exposure"), "drug_concept_id")
  expect_equal(get_concept_column("measurement"), "measurement_concept_id")
  expect_equal(get_concept_column("observation"), "observation_concept_id")
  
  # Test unknown table
  expect_equal(get_concept_column("unknown_table"), "concept_id")
})

test_that("get_date_column returns correct column names", {
  expect_equal(get_date_column("condition_occurrence"), "condition_start_date")
  expect_equal(get_date_column("procedure_occurrence"), "procedure_date")
  expect_equal(get_date_column("drug_exposure"), "drug_exposure_start_date")
  expect_equal(get_date_column("measurement"), "measurement_date")
  expect_equal(get_date_column("observation"), "observation_date")
  
  # Test unknown table
  expect_equal(get_date_column("unknown_table"), "occurrence_date")
})

test_that("create_feature_matrix handles empty input", {
  empty_features <- tibble::tibble(
    person_id = numeric(0),
    concept_id = numeric(0),
    concept_name = character(0),
    domain_id = character(0),
    occurrence_count = numeric(0),
    first_occurrence_date = as.Date(character(0)),
    last_occurrence_date = as.Date(character(0)),
    days_since_first = numeric(0),
    total_records = numeric(0)
  )
  
  result <- create_feature_matrix(empty_features)
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 0)
})

test_that("create_feature_matrix creates correct binary matrix", {
  # Create sample feature data
  sample_features <- tibble::tibble(
    person_id = c(1, 1, 2, 3),
    concept_id = c(101, 102, 101, 103),
    concept_name = c("Concept A", "Concept B", "Concept A", "Concept C"),
    domain_id = c("condition", "condition", "condition", "procedure"),
    occurrence_count = c(2, 1, 3, 1),
    first_occurrence_date = as.Date(c("2020-01-01", "2020-02-01", "2020-01-15", "2020-03-01")),
    last_occurrence_date = as.Date(c("2020-01-01", "2020-02-01", "2020-01-15", "2020-03-01")),
    days_since_first = c(0, 0, 0, 0),
    total_records = c(2, 1, 3, 1)
  )
  
  binary_matrix <- create_feature_matrix(sample_features, feature_type = "binary")
  
  expect_equal(nrow(binary_matrix), 3)  # 3 unique persons
  expect_true(all(binary_matrix %in% c(0, 1)))  # Only 0s and 1s
  expect_equal(rownames(binary_matrix), c("1", "2", "3"))
})

test_that("create_feature_matrix handles different feature types", {
  sample_features <- tibble::tibble(
    person_id = c(1, 2),
    concept_id = c(101, 101),
    concept_name = c("Concept A", "Concept A"),
    domain_id = c("condition", "condition"),
    occurrence_count = c(2, 5),
    first_occurrence_date = as.Date(c("2020-01-01", "2020-01-01")),
    last_occurrence_date = as.Date(c("2020-01-01", "2020-01-01")),
    days_since_first = c(0, 0),
    total_records = c(2, 5)
  )
  
  # Test count matrix
  count_matrix <- create_feature_matrix(sample_features, feature_type = "count")
  expect_equal(count_matrix[1, 1], 2)
  expect_equal(count_matrix[2, 1], 5)
  
  # Test log_count matrix
  log_matrix <- create_feature_matrix(sample_features, feature_type = "log_count")
  expect_equal(log_matrix[1, 1], log(2 + 1))
  expect_equal(log_matrix[2, 1], log(5 + 1))
})

test_that("create_feature_matrix handles invalid feature_type", {
  sample_features <- tibble::tibble(
    person_id = c(1),
    concept_id = c(101),
    concept_name = c("Concept A"),
    domain_id = c("condition"),
    occurrence_count = c(2),
    first_occurrence_date = as.Date("2020-01-01"),
    last_occurrence_date = as.Date("2020-01-01"),
    days_since_first = c(0),
    total_records = c(2)
  )
  
  expect_error(
    create_feature_matrix(sample_features, feature_type = "invalid"),
    "Invalid feature_type"
  )
})

test_that("export_features handles different file formats", {
  # Create temporary directory for testing
  temp_dir <- tempdir()
  
  sample_data <- tibble::tibble(
    person_id = c(1, 2),
    concept_id = c(101, 102),
    score = c(0.5, 0.8)
  )
  
  # Test CSV export
  csv_file <- file.path(temp_dir, "test_export.csv")
  result <- export_features(sample_data, csv_file, include_metadata = FALSE)
  expect_true(result)
  expect_true(file.exists(csv_file))
  
  # Test RDS export
  rds_file <- file.path(temp_dir, "test_export.rds")
  result <- export_features(sample_data, rds_file, include_metadata = TRUE)
  expect_true(result)
  expect_true(file.exists(rds_file))
  
  # Verify RDS content includes metadata
  loaded_data <- readRDS(rds_file)
  expect_true(is.list(loaded_data))
  expect_true("data" %in% names(loaded_data))
  expect_true("metadata" %in% names(loaded_data))
  
  # Clean up
  unlink(csv_file)
  unlink(rds_file)
})

test_that("export_features handles invalid file formats", {
  sample_data <- tibble::tibble(x = 1)
  
  expect_error(
    export_features(sample_data, "test.txt"),
    "Unsupported file format"
  )
})

test_that("export_features requires output_file", {
  sample_data <- tibble::tibble(x = 1)
  
  expect_error(
    export_features(sample_data, NULL),
    "output_file must be specified"
  )
  
  expect_error(
    export_features(sample_data, ""),
    "output_file must be specified"
  )
})

test_that("add_concept_names handles empty data", {
  empty_features <- tibble::tibble(
    person_id = numeric(0),
    concept_id = numeric(0),
    occurrence_count = numeric(0)
  )
  
  result <- add_concept_names(empty_features)
  expect_equal(nrow(result), 0)
  expect_true("concept_name" %in% colnames(result) || ncol(result) == 0)
})
