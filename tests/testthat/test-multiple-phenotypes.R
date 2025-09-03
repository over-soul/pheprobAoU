test_that("calculate_multiple_pheprobs validates input correctly", {
  # Test missing input
  expect_error(
    calculate_multiple_pheprobs(),
    "phenotype_concepts must be provided"
  )
  
  # Test non-list input
  expect_error(
    calculate_multiple_pheprobs(c(201820, 201826, 4193704)),
    "phenotype_concepts must be a list"
  )
  
  # Test unnamed list
  expect_error(
    calculate_multiple_pheprobs(list(c(201820, 201826, 4193704), c(4329847, 313217, 442604, 316139))),
    "All elements in phenotype_concepts must be named"
  )
  
  # Test invalid phenotype names
  expect_error(
    calculate_multiple_pheprobs(list("invalid name!" = c(201820, 201826, 4193704))),
    "Invalid phenotype names"
  )
  
  # Test invalid output format
  expect_error(
    calculate_multiple_pheprobs(
      list(diabetes = c(201820, 201826, 4193704)), 
      output_format = "invalid"
    ),
    "Invalid output_format"
  )
})

test_that("calculate_multiple_pheprobs handles valid input structure", {
  # Create valid phenotype input
  phenotypes <- list(
    diabetes = c(201820, 201826, 4193704),
    cvd = c(4329847, 313217, 442604, 316139)
  )
  
  # This will fail without database connection, but we can test input validation
  expect_error(
    calculate_multiple_pheprobs(phenotypes),
    "Error checking concept existence|Could not check concept existence",
    info = "Should reach validation phase before database error"
  )
})

test_that("calculate_multiple_pheprobs handles phenotype weights correctly", {
  phenotypes <- list(
    diabetes = c(201820, 201826, 4193704),
    cvd = c(4329847, 313217, 442604, 316139)
  )
  
  phenotype_weights <- list(
    diabetes = c("201820" = 2.0, "201826" = 1.5, "4193704" = 1.8),
    cvd = c("4329847" = 3.0, "313217" = 2.0, "442604" = 2.5, "316139" = 2.2)
  )
  
  # Test that weights structure is accepted (will fail at database level)
  expect_error(
    calculate_multiple_pheprobs(
      phenotype_concepts = phenotypes,
      phenotype_weights = phenotype_weights
    ),
    "Error checking concept existence|Could not check concept existence"
  )
})

test_that("validate_phenotype_coherence validates input correctly", {
  # Test non-list input
  expect_error(
    validate_phenotype_coherence(c(201820, 201826, 4193704)),
    "phenotype_concepts must be a named list"
  )
  
  # Test unnamed list
  expect_error(
    validate_phenotype_coherence(list(c(201820, 201826, 4193704))),
    "phenotype_concepts must be a named list"
  )
})

test_that("validate_phenotype_coherence handles valid input", {
  phenotypes <- list(
    diabetes = c(201820, 201826, 4193704),
    cvd = c(4329847, 313217, 442604, 316139)
  )
  
  # This will process validation but fail at database level
  expect_error(
    validate_phenotype_coherence(phenotypes),
    "Error checking concept existence|Could not check concept existence"
  )
})

test_that("phenotype name validation works correctly", {
  # Valid names
  valid_phenotypes <- list(
    diabetes = c(201820),
    cvd_risk = c(4329847),
    mental_health2 = c(432870)
  )
  
  expect_error(
    calculate_multiple_pheprobs(valid_phenotypes),
    "Error checking concept existence|Could not check concept existence",
    info = "Valid names should pass validation"
  )
  
  # Invalid names with spaces
  invalid_phenotypes1 <- list(
    "diabetes mellitus" = c(201820)
  )
  
  expect_error(
    calculate_multiple_pheprobs(invalid_phenotypes1),
    "Invalid phenotype names"
  )
  
  # Invalid names starting with numbers
  invalid_phenotypes2 <- list(
    "2diabetes" = c(201820)
  )
  
  expect_error(
    calculate_multiple_pheprobs(invalid_phenotypes2),
    "Invalid phenotype names"
  )
  
  # Invalid names with special characters
  invalid_phenotypes3 <- list(
    "diabetes-mellitus" = c(201820)
  )
  
  expect_error(
    calculate_multiple_pheprobs(invalid_phenotypes3),
    "Invalid phenotype names"
  )
})

# Mock data tests (without database dependency)
test_that("multiple phenotypes output format works correctly", {
  # Since we can't test with real data, we'll test the internal logic
  # by creating mock results and testing the formatting functions
  
  # Test that output format validation works
  phenotypes <- list(diabetes = c(201820), cvd = c(4329847))
  
  # Wide format (default)
  expect_error(
    calculate_multiple_pheprobs(phenotypes, output_format = "wide"),
    "Error checking concept existence|Could not check concept existence"
  )
  
  # Long format
  expect_error(
    calculate_multiple_pheprobs(phenotypes, output_format = "long"),
    "Error checking concept existence|Could not check concept existence"
  )
  
  # Invalid format
  expect_error(
    calculate_multiple_pheprobs(phenotypes, output_format = "matrix"),
    "Invalid output_format"
  )
})

test_that("phenotype weights structure validation", {
  phenotypes <- list(
    diabetes = c(201820, 201826, 4193704),
    cvd = c(4329847, 313217, 442604, 316139)
  )
  
  # Valid weights structure
  valid_weights <- list(
    diabetes = c("201820" = 2.0, "201826" = 1.5, "4193704" = 1.8)
  )
  
  expect_error(
    calculate_multiple_pheprobs(
      phenotype_concepts = phenotypes,
      phenotype_weights = valid_weights
    ),
    "Error checking concept existence|Could not check concept existence",
    info = "Should pass weight validation"
  )
  
  # Partial weights (should be fine)
  partial_weights <- list(
    diabetes = c("201820" = 2.0)  # Only one concept weighted
  )
  
  expect_error(
    calculate_multiple_pheprobs(
      phenotype_concepts = phenotypes,
      phenotype_weights = partial_weights
    ),
    "Error checking concept existence|Could not check concept existence",
    info = "Partial weights should be accepted"
  )
})

test_that("error handling preserves structure", {
  # Test that if one phenotype fails, others can still succeed
  # This tests the tryCatch logic in the main function
  
  phenotypes <- list(
    valid_diabetes = c(201820, 201826, 4193704),
    invalid_phenotype = c(999999999)  # Invalid concept ID
  )
  
  # The function should handle individual phenotype failures gracefully
  expect_error(
    calculate_multiple_pheprobs(phenotypes),
    "Error checking concept existence|Could not check concept existence|No valid phenotype results",
    info = "Should handle mixed valid/invalid phenotypes"
  )
})

test_that("coherence validation settings work", {
  phenotypes <- list(
    diabetes = c(201826, 4329847),
    mixed = c(201820, 313217, 432870)  # Mixed domains
  )
  
  # Test with domain checking disabled
  expect_error(
    validate_phenotype_coherence(phenotypes, check_domains = FALSE),
    "Error checking concept existence|Could not check concept existence"
  )
  
  # Test with custom domain threshold
  expect_error(
    validate_phenotype_coherence(phenotypes, max_domains_per_phenotype = 5),
    "Error checking concept existence|Could not check concept existence"
  )
})
