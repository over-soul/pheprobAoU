test_that("calculate_pheprob validates inputs correctly", {
  # Test missing concept_ids
  expect_error(
    calculate_pheprob(NULL),
    "concept_ids must be provided"
  )
  
  # Test empty concept_ids
  expect_error(
    calculate_pheprob(c()),
    "concept_ids must be provided"
  )
  
  # Test invalid method
  expect_error(
    calculate_pheprob(c(201826), method = "invalid_method"),
    "Invalid method"
  )
  
  # Test invalid normalization
  expect_error(
    calculate_pheprob(c(201826), normalization = "invalid_norm"),
    "Invalid normalization"
  )
  
  # Test invalid output_format
  expect_error(
    calculate_pheprob(c(201826), output_format = "invalid_format"),
    "Invalid output_format"
  )
})

test_that("create_feature_matrix creates correct structure", {
  # Create mock feature data
  mock_features <- tibble::tibble(
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
  
  result <- create_feature_matrix(mock_features, feature_type = "binary")
  
  expect_true(is.matrix(result))
  expect_equal(nrow(result), 3)  # 3 unique persons
  expect_true(all(result %in% c(0, 1)))  # Binary values
})

test_that("binomial mixture model validation works", {
  # Test basic input validation for binomial mixture
  expect_error(
    validate_binomial_mixture_inputs(c(1, 2), c(1)),  # Different lengths
    "S and C must have the same length"
  )
  
  expect_error(
    validate_binomial_mixture_inputs(c(2, 1), c(1, 2)),  # S > C violation
    "Disease-relevant codes.*cannot exceed total codes"
  )
  
  expect_error(
    validate_binomial_mixture_inputs(c(), c()),  # Empty input
    "S and C cannot be empty"
  )
})

test_that("fit_pheprob_binomial_mixture handles basic data", {
  # Create simple test data
  S <- c(1, 2, 3, 1, 0)  # Disease-relevant codes
  C <- c(10, 15, 20, 8, 5)  # Total codes
  
  # Fit model with minimal iterations for testing
  result <- fit_pheprob_binomial_mixture(S, C, max_iterations = 10, verbose = FALSE)
  
  expect_true(is.list(result))
  expect_true("parameters" %in% names(result))
  expect_true("phenotype_probabilities" %in% names(result))
  expect_equal(length(result$phenotype_probabilities), length(S))
  expect_true(all(result$phenotype_probabilities >= 0 & result$phenotype_probabilities <= 1))
})

test_that("validate_binomial_data_quality works correctly", {
  # Create test data
  test_data <- tibble::tibble(
    person_id = c(1, 2, 3),
    S = c(1, 2, 1),
    C = c(10, 15, 8)
  )
  
  result <- validate_binomial_data_quality(test_data)
  
  expect_true(is.list(result))
  expect_true("summary_stats" %in% names(result))
  expect_true("overall_quality" %in% names(result))
  expect_true("score" %in% names(result$overall_quality))
  expect_true(result$overall_quality$score >= 0 && result$overall_quality$score <= 100)
})

test_that("SQL injection protection works", {
  # Test that invalid person_ids are rejected
  expect_error(
    create_total_count_query("condition_occurrence", 
                           person_ids = c("'; DROP TABLE person; --"), 
                           NULL, NULL),
    "Invalid person_ids: must be positive integers"
  )
  
  expect_error(
    create_total_count_query("condition_occurrence", 
                           person_ids = c(-1, 0), 
                           NULL, NULL),
    "Invalid person_ids: must be positive integers"
  )
})

test_that("validate_phenotype_coherence works correctly", {
  # Valid phenotype list
  valid_phenotypes <- list(
    diabetes = c(201826, 4329847),
    cvd = c(314866, 313217)
  )
  
  # This will fail due to database connection, but we can test structure
  expect_error(
    validate_phenotype_coherence(valid_phenotypes),
    "Could not check concept existence|Error checking concept existence"
  )
  
  # Invalid phenotype names
  invalid_phenotypes <- list(
    "invalid name!" = c(201826)
  )
  
  expect_error(
    validate_phenotype_coherence(invalid_phenotypes),
    "Invalid phenotype names"
  )
})
