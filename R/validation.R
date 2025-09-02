#' Validate OMOP Concept IDs
#'
#' Validates a vector of OMOP concept IDs to ensure they exist in the All of Us
#' database and are of the correct format.
#'
#' @param concept_ids A numeric vector of OMOP concept IDs to validate
#' @param check_existence Logical, whether to check if concept IDs exist in the
#'   All of Us database (default: TRUE). Requires active database connection.
#'
#' @return A list containing:
#'   \item{valid}{Logical vector indicating which concept IDs are valid}
#'   \item{invalid_ids}{Vector of invalid concept IDs}
#'   \item{missing_ids}{Vector of concept IDs not found in database}
#'   \item{summary}{Summary statistics of validation results}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Validate concept IDs
#' concept_ids <- c(201826, 4329847, 9999999)  # Last one is invalid
#' validation_result <- validate_concept_ids(concept_ids)
#' print(validation_result$summary)
#' }
validate_concept_ids <- function(concept_ids, check_existence = TRUE) {
  
  # Check if concept_ids is provided and not empty
  if (missing(concept_ids) || is.null(concept_ids) || length(concept_ids) == 0) {
    cli::cli_abort("concept_ids must be provided and cannot be empty")
  }
  
  # Convert to numeric if not already
  if (!is.numeric(concept_ids)) {
    tryCatch({
      concept_ids <- as.numeric(concept_ids)
    }, error = function(e) {
      cli::cli_abort("concept_ids must be numeric or convertible to numeric")
    })
  }
  
  # Check for NA values
  na_indices <- is.na(concept_ids)
  if (any(na_indices)) {
    cli::cli_alert_info("Removing {sum(na_indices)} NA values from concept_ids")
    concept_ids <- concept_ids[!na_indices]
  }
  
  # Check for non-positive values
  non_positive <- concept_ids <= 0
  invalid_ids <- concept_ids[non_positive]
  valid_format_ids <- concept_ids[!non_positive]
  
  # Initialize results
  missing_ids <- c()
  valid_existence <- rep(TRUE, length(valid_format_ids))
  
  # Check existence in database if requested
  if (check_existence && length(valid_format_ids) > 0) {
    tryCatch({
      # Query the concept table to check existence
      existing_concepts <- allofus::aou_sql(
        glue::glue("
          SELECT DISTINCT concept_id 
          FROM concept 
          WHERE concept_id IN ({paste(valid_format_ids, collapse = ', ')})
        ")
      )
      
      if (nrow(existing_concepts) > 0) {
        existing_ids <- existing_concepts$concept_id
        missing_ids <- setdiff(valid_format_ids, existing_ids)
        valid_existence <- valid_format_ids %in% existing_ids
      } else {
        missing_ids <- valid_format_ids
        valid_existence <- rep(FALSE, length(valid_format_ids))
      }
      
    }, error = function(e) {
      cli::cli_alert_info("Could not check concept existence in database: {e$message}")
      cli::cli_alert_info("Proceeding without existence validation")
      check_existence <- FALSE
    })
  }
  
  # Combine validation results
  all_valid <- rep(FALSE, length(concept_ids))
  all_valid[!non_positive] <- valid_existence
  
  # Create summary
  n_total <- length(concept_ids) + sum(na_indices)
  n_valid <- sum(all_valid)
  n_invalid_format <- length(invalid_ids)
  n_missing <- length(missing_ids)
  n_na <- sum(na_indices)
  
  summary_stats <- list(
    total_submitted = n_total,
    valid_concept_ids = n_valid,
    invalid_format = n_invalid_format,
    missing_from_database = n_missing,
    na_values_removed = n_na,
    validation_rate = round(n_valid / (n_total - n_na) * 100, 2)
  )
  
  # Return results
  return(list(
    valid = all_valid,
    invalid_ids = invalid_ids,
    missing_ids = missing_ids,
    summary = summary_stats,
    validated_concept_ids = concept_ids[all_valid]
  ))
}


#' Validate Person IDs
#'
#' Validates a vector of person IDs to ensure they exist in the All of Us
#' person table.
#'
#' @param person_ids A numeric vector of person IDs to validate. If NULL,
#'   validation is skipped.
#'
#' @return A list containing validation results similar to validate_concept_ids
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Validate person IDs
#' person_ids <- c(1, 2, 3, 999999)
#' validation_result <- validate_person_ids(person_ids)
#' }
validate_person_ids <- function(person_ids) {
  
  if (is.null(person_ids)) {
    return(list(
      valid = logical(0),
      invalid_ids = numeric(0),
      missing_ids = numeric(0),
      summary = list(
        total_submitted = 0,
        valid_person_ids = 0,
        validation_rate = 100
      ),
      validated_person_ids = numeric(0)
    ))
  }
  
  # Convert to numeric if not already
  if (!is.numeric(person_ids)) {
    tryCatch({
      person_ids <- as.numeric(person_ids)
    }, error = function(e) {
      cli::cli_abort("person_ids must be numeric or convertible to numeric")
    })
  }
  
  # Remove duplicates and NA values
  original_length <- length(person_ids)
  person_ids <- unique(person_ids[!is.na(person_ids)])
  
  if (length(person_ids) < original_length) {
    cli::cli_alert_info("Removed duplicates and NA values from person_ids")
  }
  
  # Check existence in person table
  missing_ids <- c()
  valid_existence <- rep(TRUE, length(person_ids))
  
  tryCatch({
    # Query person table in batches for large datasets
    batch_size <- 1000
    existing_ids <- c()
    
    for (i in seq(1, length(person_ids), batch_size)) {
      batch_end <- min(i + batch_size - 1, length(person_ids))
      batch_ids <- person_ids[i:batch_end]
      
      batch_result <- allofus::aou_sql(
        glue::glue("
          SELECT DISTINCT person_id 
          FROM person 
          WHERE person_id IN ({paste(batch_ids, collapse = ', ')})
        ")
      )
      
      if (nrow(batch_result) > 0) {
        existing_ids <- c(existing_ids, batch_result$person_id)
      }
    }
    
    missing_ids <- setdiff(person_ids, existing_ids)
    valid_existence <- person_ids %in% existing_ids
    
  }, error = function(e) {
    cli::cli_abort("Error checking person IDs in database: {e$message}")
  })
  
  # Create summary
  n_total <- original_length
  n_valid <- sum(valid_existence)
  n_missing <- length(missing_ids)
  
  summary_stats <- list(
    total_submitted = n_total,
    valid_person_ids = n_valid,
    missing_from_database = n_missing,
    validation_rate = round(n_valid / length(person_ids) * 100, 2)
  )
  
  return(list(
    valid = valid_existence,
    invalid_ids = numeric(0),  # No format validation for person IDs
    missing_ids = missing_ids,
    summary = summary_stats,
    validated_person_ids = person_ids[valid_existence]
  ))
}


#' Validate Function Parameters
#'
#' Internal function to validate common parameters across package functions.
#'
#' @param concept_ids Numeric vector of OMOP concept IDs
#' @param person_ids Numeric vector of person IDs (can be NULL)
#' @param output_file Character string for output file path (can be NULL)
#' @param ... Additional arguments
#'
#' @return List with validated parameters
#'
#' @keywords internal
validate_parameters <- function(concept_ids, person_ids = NULL, output_file = NULL, ...) {
  
  # Validate concept IDs
  concept_validation <- validate_concept_ids(concept_ids)
  
  if (concept_validation$summary$valid_concept_ids == 0) {
    cli::cli_abort("No valid concept IDs found. Please check your input.")
  }
  
  # Validate person IDs if provided
  person_validation <- validate_person_ids(person_ids)
  
  # Validate output file if provided
  if (!is.null(output_file)) {
    if (!is.character(output_file) || length(output_file) != 1) {
      cli::cli_abort("output_file must be a single character string")
    }
    
    # Check if directory exists
    output_dir <- dirname(output_file)
    if (!dir.exists(output_dir)) {
      cli::cli_abort("Output directory does not exist: {output_dir}")
    }
  }
  
  return(list(
    concept_validation = concept_validation,
    person_validation = person_validation,
    validated_concept_ids = concept_validation$validated_concept_ids,
    validated_person_ids = person_validation$validated_person_ids,
    output_file = output_file
  ))
}
