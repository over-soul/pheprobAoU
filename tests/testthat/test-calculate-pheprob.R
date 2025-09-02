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

test_that("calculate_simple_scores creates correct structure", {
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
  
  concept_ids <- c(101, 102, 103)
  result <- calculate_simple_scores(mock_features, concept_ids)
  
  expect_true(is.data.frame(result))
  expect_true("person_id" %in% colnames(result))
  expect_true("pheprob_score" %in% colnames(result))
  expect_equal(nrow(result), 3)  # 3 unique persons
})

test_that("calculate_weighted_scores handles log transformation", {
  mock_features <- tibble::tibble(
    person_id = c(1, 2),
    concept_id = c(101, 101),
    concept_name = c("Concept A", "Concept A"),
    domain_id = c("condition", "condition"),
    occurrence_count = c(1, 10),  # Different counts
    first_occurrence_date = as.Date(c("2020-01-01", "2020-01-01")),
    last_occurrence_date = as.Date(c("2020-01-01", "2020-01-01")),
    days_since_first = c(0, 0),
    total_records = c(1, 10)
  )
  
  concept_ids <- c(101)
  result <- calculate_weighted_scores(mock_features, concept_ids)
  
  expect_true(result$pheprob_score[2] > result$pheprob_score[1])  # Higher count should have higher score
})

test_that("apply_concept_weights modifies scores correctly", {
  # Create a simple feature matrix
  test_matrix <- matrix(c(1, 1, 2, 2), nrow = 2, ncol = 2)
  colnames(test_matrix) <- c("101_ConceptA", "102_ConceptB")
  rownames(test_matrix) <- c("1", "2")
  
  # Apply weights
  weights <- c("101" = 2.0, "102" = 0.5)
  weighted_matrix <- apply_concept_weights(test_matrix, weights)
  
  expect_equal(weighted_matrix[1, 1], 2)  # 1 * 2.0
  expect_equal(weighted_matrix[1, 2], 1)  # 2 * 0.5
  expect_equal(weighted_matrix[2, 1], 2)  # 1 * 2.0
  expect_equal(weighted_matrix[2, 2], 1)  # 2 * 0.5
})

test_that("apply_concept_weights handles NULL weights", {
  test_matrix <- matrix(c(1, 2), nrow = 1, ncol = 2)
  colnames(test_matrix) <- c("101_ConceptA", "102_ConceptB")
  
  result <- apply_concept_weights(test_matrix, NULL)
  expect_equal(result, test_matrix)
  
  result <- apply_concept_weights(test_matrix, c())
  expect_equal(result, test_matrix)
})

test_that("normalize_scores applies different normalization methods", {
  scores_tibble <- tibble::tibble(
    person_id = c(1, 2, 3),
    pheprob_score = c(1, 5, 10)
  )
  
  # Test minmax normalization
  minmax_result <- normalize_scores(scores_tibble, "minmax")
  expect_equal(minmax_result$pheprob_score, c(0, 4/9, 1))
  
  # Test zscore normalization
  zscore_result <- normalize_scores(scores_tibble, "zscore")
  mean_score <- mean(c(1, 5, 10))
  sd_score <- sd(c(1, 5, 10))
  expected_zscore <- (c(1, 5, 10) - mean_score) / sd_score
  expect_equal(zscore_result$pheprob_score, expected_zscore)
  
  # Test logistic normalization
  logistic_result <- normalize_scores(scores_tibble, "logistic")
  expected_logistic <- 1 / (1 + exp(-c(1, 5, 10)))
  expect_equal(logistic_result$pheprob_score, expected_logistic)
})

test_that("normalize_scores handles edge cases", {
  # Test identical scores (no variance)
  identical_scores <- tibble::tibble(
    person_id = c(1, 2, 3),
    pheprob_score = c(5, 5, 5)
  )
  
  # Minmax with no range should return zeros
  minmax_result <- normalize_scores(identical_scores, "minmax")
  expect_equal(minmax_result$pheprob_score, c(0, 0, 0))
  
  # Z-score with no variance should return zeros
  zscore_result <- normalize_scores(identical_scores, "zscore")
  expect_equal(zscore_result$pheprob_score, c(0, 0, 0))
})

test_that("format_pheprob_output creates correct formats", {
  scores <- tibble::tibble(
    person_id = c(1, 2),
    pheprob_score = c(0.5, 0.8),
    "101_ConceptA" = c(1, 0),
    "102_ConceptB" = c(0, 1)
  )
  
  concept_ids <- c(101, 102)
  
  # Test matrix format
  matrix_result <- format_pheprob_output(scores, "matrix", concept_ids)
  expect_true(is.matrix(matrix_result))
  expect_equal(nrow(matrix_result), 2)
  expect_equal(rownames(matrix_result), c("1", "2"))
  
  # Test wide format (should return as-is)
  wide_result <- format_pheprob_output(scores, "wide", concept_ids)
  expect_equal(wide_result, scores)
})

test_that("create_empty_scores_tibble has correct structure", {
  concept_ids <- c(101, 102, 103)
  empty_scores <- create_empty_scores_tibble(concept_ids)
  
  expect_true(is.data.frame(empty_scores))
  expect_true("person_id" %in% colnames(empty_scores))
  expect_true("pheprob_score" %in% colnames(empty_scores))
  expect_equal(nrow(empty_scores), 0)
})

test_that("create_empty_output handles different formats", {
  concept_ids <- c(101, 102)
  
  # Test matrix format
  empty_matrix <- create_empty_output("matrix", concept_ids)
  expect_true(is.matrix(empty_matrix))
  expect_equal(nrow(empty_matrix), 0)
  expect_equal(ncol(empty_matrix), 2)
  
  # Test long format
  empty_long <- create_empty_output("long", concept_ids)
  expect_true(is.data.frame(empty_long))
  expect_true(all(c("person_id", "concept_id", "concept_label", "concept_score", "pheprob_score") %in% colnames(empty_long)))
  expect_equal(nrow(empty_long), 0)
  
  # Test wide format
  empty_wide <- create_empty_output("wide", concept_ids)
  expect_true(is.data.frame(empty_wide))
  expect_true(all(c("person_id", "pheprob_score") %in% colnames(empty_wide)))
  expect_equal(nrow(empty_wide), 0)
})

test_that("combine_batch_results handles empty results", {
  batch_results <- list(
    tibble::tibble(person_id = numeric(0), pheprob_score = numeric(0)),
    tibble::tibble(person_id = numeric(0), pheprob_score = numeric(0))
  )
  
  result <- combine_batch_results(batch_results)
  expect_equal(nrow(result), 0)
  expect_true(all(c("person_id", "pheprob_score") %in% colnames(result)))
})

test_that("combine_batch_results combines valid results", {
  batch_results <- list(
    tibble::tibble(person_id = c(1, 2), pheprob_score = c(0.5, 0.8)),
    tibble::tibble(person_id = c(3, 4), pheprob_score = c(0.3, 0.9)),
    tibble::tibble(person_id = numeric(0), pheprob_score = numeric(0))  # Empty batch
  )
  
  result <- combine_batch_results(batch_results)
  expect_equal(nrow(result), 4)
  expect_equal(result$person_id, c(1, 2, 3, 4))
  expect_equal(result$pheprob_score, c(0.5, 0.8, 0.3, 0.9))
})
