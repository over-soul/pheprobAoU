test_that("validate_concept_ids handles basic validation correctly", {
  # Test valid concept IDs
  valid_ids <- c(201820, 201826, 4193704)
  
  # Mock validation without database check
  result <- validate_concept_ids(valid_ids, check_existence = FALSE)
  
  expect_true(all(result$valid))
  expect_equal(length(result$validated_concept_ids), 3)
  expect_equal(result$summary$total_submitted, 3)
  expect_equal(result$summary$valid_concept_ids, 3)
})

test_that("validate_concept_ids rejects invalid inputs", {
  # Test empty input
  expect_error(validate_concept_ids(NULL), "concept_ids must be provided")
  expect_error(validate_concept_ids(c()), "concept_ids must be provided")
  
  # Test non-numeric input that can't be converted
  expect_error(validate_concept_ids(c("invalid", "text")), "concept_ids must be numeric")
  
  # Test negative values
  result <- validate_concept_ids(c(-1, 0, 1, 2), check_existence = FALSE)
  expect_equal(length(result$invalid_ids), 2)  # -1 and 0
  expect_equal(length(result$validated_concept_ids), 2)  # 1 and 2
})

test_that("validate_concept_ids handles NA values", {
  # Test with NA values
  result <- validate_concept_ids(c(1, NA, 2, NA, 3), check_existence = FALSE)
  
  expect_equal(result$summary$na_values_removed, 2)
  expect_equal(length(result$validated_concept_ids), 3)
})

test_that("validate_concept_ids converts character numbers correctly", {
  # Test character numbers that can be converted
  result <- validate_concept_ids(c("123", "456", "789"), check_existence = FALSE)
  
  expect_true(all(result$valid))
  expect_equal(result$validated_concept_ids, c(123, 456, 789))
})

test_that("validate_person_ids handles NULL input", {
  result <- validate_person_ids(NULL)
  
  expect_equal(length(result$valid), 0)
  expect_equal(result$summary$total_submitted, 0)
  expect_equal(result$summary$validation_rate, 100)
})

test_that("validate_person_ids handles valid inputs", {
  # Since we can't test database connectivity, we'll test the structure
  # This would normally require mocking the database connection
  person_ids <- c(1, 2, 3)
  
  # We can't actually test the database query without a connection
  # So we'll just test that the function doesn't error on valid input format
  expect_error(validate_person_ids(person_ids), NA, info = "Function should handle numeric input")
})

test_that("validate_parameters combines validations correctly", {
  # Test basic parameter validation
  concept_ids <- c(201820, 201826, 4193704)
  
  # This will fail without database connection, but we can test error handling
  expect_error(
    validate_parameters(concept_ids, person_ids = NULL),
    "Error checking concept existence|Could not check concept existence",
    info = "Should handle database connection errors gracefully"
  )
})

test_that("validate_parameters handles output file validation", {
  concept_ids <- c(201820, 201826, 4193704)
  
  # Test invalid output file
  expect_error(
    validate_parameters(concept_ids, output_file = c("file1.csv", "file2.csv")),
    "output_file must be a single character string"
  )
  
  expect_error(
    validate_parameters(concept_ids, output_file = 123),
    "output_file must be a single character string"
  )
})
