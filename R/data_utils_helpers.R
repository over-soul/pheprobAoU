#' Helper Functions for Data Extraction (Sinnott et al., 2018)
#'
#' This file contains helper functions for data extraction required for
#' the PheProb binomial mixture model implementation (Sinnott et al., 2018).
#'
#' @name data_utils_helpers
NULL

#' Extract Domain-Specific Total Counts
#'
#' Extracts total billing code counts from a specific OMOP domain table.
#'
#' @param table_name Name of the OMOP table (e.g., "condition_occurrence")
#' @param person_ids Vector of person IDs to include
#' @param date_range Date range filter
#' @param exclude_concepts Concept IDs to exclude
#' @param max_persons Maximum number of persons to process
#'
#' @return Tibble with person-level counts for this domain
#'
#' @keywords internal
extract_domain_total_counts <- function(table_name,
                                       person_ids = NULL,
                                       date_range = NULL,
                                       exclude_concepts = NULL,
                                       max_persons = NULL) {
  
  # This is a placeholder implementation - in real usage, this would query the All of Us database
  # For now, we'll simulate the database query structure
  
  tryCatch({
    
    # Construct base query for total counts
    base_query <- create_total_count_query(table_name, person_ids, date_range, exclude_concepts)
    
    # Execute query (placeholder - would use allofus package in real implementation)
    domain_result <- execute_total_count_query(base_query, max_persons)
    
    # Format results
    if (nrow(domain_result) > 0) {
      formatted_result <- domain_result %>%
        dplyr::select(
          .data$person_id,
          domain_count = .data$total_count,
          first_date = .data$first_occurrence,
          last_date = .data$last_occurrence
        ) %>%
        dplyr::mutate(
          domain = extract_domain_from_table(table_name)
        )
      
      return(formatted_result)
    } else {
      return(tibble::tibble(
        person_id = integer(0),
        domain_count = integer(0),
        first_date = as.Date(character(0)),
        last_date = as.Date(character(0)),
        domain = character(0)
      ))
    }
    
  }, error = function(e) {
    cli::cli_alert_warning("Failed to extract from {table_name}: {e$message}")
    return(tibble::tibble(
      person_id = integer(0),
      domain_count = integer(0),
      first_date = as.Date(character(0)),
      last_date = as.Date(character(0)),
      domain = character(0)
    ))
  })
}

#' Check All of Us Connection Status
#'
#' Checks if the package is connected to All of Us Research Program database.
#' Only works within All of Us Research Workbench environment.
#'
#' @return Logical indicating if connected
#' @export
#'
#' @examples
#' \dontrun{
#' if (is_aou_connected()) {
#'   # Run analysis
#' } else {
#'   connect_aou()
#' }
#' }
is_aou_connected <- function() {
  tryCatch({
    if (requireNamespace("allofus", quietly = TRUE)) {
      # Test connection with a simple query
      result <- allofus::aou_sql("SELECT 1 LIMIT 1")
      return(nrow(result) > 0)
    }
    return(FALSE)
  }, error = function(e) {
    return(FALSE)
  })
}

#' Connect to All of Us Research Program
#'
#' Convenience function to establish All of Us database connection with status checking.
#' Only works within All of Us Research Workbench environment.
#'
#' @param verbose Logical indicating whether to show connection status messages
#' @return Logical indicating if connection was successful
#' @export
#'
#' @examples
#' \dontrun{
#' # Connect with status messages (only works in All of Us environment)
#' connect_aou()
#' 
#' # Connect silently
#' connect_aou(verbose = FALSE)
#' }
connect_aou <- function(verbose = TRUE) {
  
  if (!requireNamespace("allofus", quietly = TRUE)) {
    if (verbose) {
      cli::cli_alert_danger("allofus package is required but not available")
      cli::cli_alert_info("Install with: install.packages('allofus')")
    }
    return(FALSE)
  }
  
  # Check if we're in an All of Us environment
  # Note: Using a simple check here since is_aou_environment is internal
  in_aou_env <- any(nzchar(c(
    Sys.getenv("WORKSPACE_NAMESPACE"),
    Sys.getenv("GOOGLE_CLOUD_PROJECT")
  )))
  
  if (!in_aou_env) {
    if (verbose) {
      cli::cli_alert_warning("Not in All of Us Research Workbench environment")
      cli::cli_alert_info("Database connection only available within All of Us cloud")
      cli::cli_alert_info("For local development, use simulation or mock data functions")
    }
    return(FALSE)
  }
  
  # Check if already connected
  if (is_aou_connected()) {
    if (verbose) {
      cli::cli_alert_success("Already connected to All of Us Research Program")
    }
    return(TRUE)
  }
  
  # Attempt connection
  if (verbose) {
    cli::cli_alert_info("Connecting to All of Us Research Program...")
  }
  
  tryCatch({
    allofus::aou_connect()
    
    # Verify connection
    if (is_aou_connected()) {
      if (verbose) {
        cli::cli_alert_success("Successfully connected to All of Us Research Program")
      }
      return(TRUE)
    } else {
      if (verbose) {
        cli::cli_alert_warning("Connection attempted but verification failed")
      }
      return(FALSE)
    }
    
  }, error = function(e) {
    if (verbose) {
      cli::cli_alert_danger("Failed to connect: {e$message}")
      cli::cli_alert_info("Ensure you're in All of Us Research Workbench environment")
    }
    return(FALSE)
  })
}

#' @keywords internal
extract_domain_total_counts <- function(table_name,
                                       person_ids = NULL,
                                       date_range = NULL,
                                       exclude_concepts = NULL,
                                       max_persons = NULL) {
  
  # This is a placeholder implementation - in real usage, this would query the All of Us database
  # For now, we'll simulate the database query structure
  
  tryCatch({
    
    # Construct base query for total counts
    base_query <- create_total_count_query(table_name, person_ids, date_range, exclude_concepts)
    
    # Execute query (placeholder - would use allofus package in real implementation)
    domain_result <- execute_total_count_query(base_query, max_persons)
    
    # Format results
    if (nrow(domain_result) > 0) {
      formatted_result <- domain_result %>%
        dplyr::select(
          .data$person_id,
          domain_count = .data$total_count,
          first_date = .data$first_occurrence,
          last_date = .data$last_occurrence
        ) %>%
        dplyr::mutate(
          domain = extract_domain_from_table(table_name)
        )
      
      return(formatted_result)
    } else {
      return(tibble::tibble(
        person_id = integer(0),
        domain_count = integer(0),
        first_date = as.Date(character(0)),
        last_date = as.Date(character(0)),
        domain = character(0)
      ))
    }
    
  }, error = function(e) {
    cli::cli_alert_warning("Failed to extract from {table_name}: {e$message}")
    return(tibble::tibble(
      person_id = integer(0),
      domain_count = integer(0),
      first_date = as.Date(character(0)),
      last_date = as.Date(character(0)),
      domain = character(0)
    ))
  })
}

#' Create Total Count Query
#'
#' Creates SQL query for extracting total billing code counts from OMOP tables.
#'
#' @param table_name OMOP table name
#' @param person_ids Person ID filter
#' @param date_range Date range filter
#' @param exclude_concepts Concepts to exclude
#'
#' @return SQL query string
#'
#' @keywords internal
create_total_count_query <- function(table_name, person_ids, date_range, exclude_concepts) {
  
  # Determine date column name based on table
  date_column <- switch(table_name,
    "condition_occurrence" = "condition_start_date",
    "procedure_occurrence" = "procedure_date", 
    "drug_exposure" = "drug_exposure_start_date",
    "measurement" = "measurement_date",
    "observation" = "observation_date",
    "visit_occurrence" = "visit_start_date"
  )
  
  # Determine concept column name
  concept_column <- switch(table_name,
    "condition_occurrence" = "condition_concept_id",
    "procedure_occurrence" = "procedure_concept_id",
    "drug_exposure" = "drug_concept_id", 
    "measurement" = "measurement_concept_id",
    "observation" = "observation_concept_id",
    "visit_occurrence" = "visit_concept_id"
  )
  
  # Build query components
  select_clause <- glue::glue("
    SELECT 
      person_id,
      COUNT(*) as total_count,
      MIN({date_column}) as first_occurrence,
      MAX({date_column}) as last_occurrence
  ")
  
  from_clause <- glue::glue("FROM {table_name}")
  
  where_conditions <- character(0)
  
  # Person ID filter
  if (!is.null(person_ids) && length(person_ids) > 0) {
    # Validate person_ids to prevent SQL injection
    if (!all(is.numeric(person_ids)) || any(is.na(person_ids)) || any(person_ids <= 0)) {
      cli::cli_abort("Invalid person_ids: must be positive integers")
    }
    # Convert to integer to ensure safety
    safe_person_ids <- as.integer(person_ids)
    person_list <- paste(safe_person_ids, collapse = ", ")
    where_conditions <- c(where_conditions, glue::glue("person_id IN ({person_list})"))
  }
  
  # Date range filter
  if (!is.null(date_range)) {
    if (!is.null(date_range$start)) {
      # Validate and format date safely
      if (!inherits(date_range$start, "Date")) {
        cli::cli_abort("date_range$start must be a Date object")
      }
      safe_start_date <- as.character(date_range$start)
      where_conditions <- c(where_conditions, glue::glue("{date_column} >= '{safe_start_date}'"))
    }
    if (!is.null(date_range$end)) {
      # Validate and format date safely
      if (!inherits(date_range$end, "Date")) {
        cli::cli_abort("date_range$end must be a Date object")
      }
      safe_end_date <- as.character(date_range$end)
      where_conditions <- c(where_conditions, glue::glue("{date_column} <= '{safe_end_date}'"))
    }
  }
  
  # Exclude concepts filter
  if (!is.null(exclude_concepts) && length(exclude_concepts) > 0) {
    # Validate exclude_concepts to prevent SQL injection
    if (!all(is.numeric(exclude_concepts)) || any(is.na(exclude_concepts)) || any(exclude_concepts <= 0)) {
      cli::cli_abort("Invalid exclude_concepts: must be positive integers")
    }
    # Convert to integer to ensure safety
    safe_exclude_concepts <- as.integer(exclude_concepts)
    exclude_list <- paste(safe_exclude_concepts, collapse = ", ")
    where_conditions <- c(where_conditions, glue::glue("{concept_column} NOT IN ({exclude_list})"))
  }
  
  # Combine WHERE conditions
  where_clause <- if (length(where_conditions) > 0) {
    paste("WHERE", paste(where_conditions, collapse = " AND "))
  } else {
    ""
  }
  
  group_clause <- "GROUP BY person_id"
  order_clause <- "ORDER BY person_id"
  
  # Combine all parts
  query <- paste(select_clause, from_clause, where_clause, group_clause, order_clause, sep = "\n")
  
  return(query)
}

#' Execute Total Count Query
#'
#' Executes the total count query against the All of Us database.
#'
#' @param query SQL query string
#' @param max_persons Maximum persons to return
#'
#' @return Query results tibble
#'
#' @keywords internal
execute_total_count_query <- function(query, max_persons = NULL) {
  
  # Execute query using All of Us database connection
  tryCatch({
    
    # Execute the SQL query using allofus package
    result <- allofus::aou_sql(query)
    
    if (nrow(result) > 0) {
      # Convert date columns to proper Date type
      result$first_occurrence <- as.Date(result$first_occurrence)
      result$last_occurrence <- as.Date(result$last_occurrence)
      
      # Apply max_persons limit if specified (for testing/development)
      if (!is.null(max_persons) && is.numeric(max_persons) && max_persons > 0) {
        if (nrow(result) > max_persons) {
          result <- result[1:max_persons, ]
          cli::cli_alert_info("Limited results to {max_persons} persons for testing")
        }
      }
      
      return(tibble::as_tibble(result))
    } else {
      cli::cli_alert_warning("No healthcare utilization data found")
      return(tibble::tibble(
        person_id = integer(0),
        total_count = integer(0),
        first_occurrence = as.Date(character(0)),
        last_occurrence = as.Date(character(0))
      ))
    }
    
  }, error = function(e) {
    cli::cli_alert_warning("Database query failed: {e$message}")
    return(tibble::tibble(
      person_id = integer(0),
      total_count = integer(0),
      first_occurrence = as.Date(character(0)),
      last_occurrence = as.Date(character(0))
    ))
  })
}

#' Combine Domain Counts
#'
#' Combines counts from multiple domains into a single total utilization tibble.
#'
#' @param domain_counts List of domain-specific count tibbles
#' @param domains Vector of domain names
#'
#' @return Combined utilization tibble
#'
#' @keywords internal
combine_domain_counts <- function(domain_counts, domains) {
  
  # Get all unique person IDs
  all_person_ids <- unique(unlist(lapply(domain_counts, function(x) x$person_id)))
  
  # Initialize result with all persons
  result <- tibble::tibble(person_id = all_person_ids)
  
  # Add domain-specific counts
  for (domain in domains) {
    if (domain %in% names(domain_counts)) {
      domain_data <- domain_counts[[domain]] %>%
        dplyr::select(.data$person_id, !!paste0(domain, "_count") := .data$domain_count,
                     !!paste0(domain, "_first") := .data$first_date,
                     !!paste0(domain, "_last") := .data$last_date)
      
      result <- result %>%
        dplyr::left_join(domain_data, by = "person_id")
    } else {
      # Add zeros for missing domains
      result[[paste0(domain, "_count")]] <- 0L
      result[[paste0(domain, "_first")]] <- as.Date(NA)
      result[[paste0(domain, "_last")]] <- as.Date(NA)
    }
  }
  
  # Replace NA counts with 0
  count_cols <- paste0(domains, "_count")
  result[count_cols] <- lapply(result[count_cols], function(x) ifelse(is.na(x), 0L, x))
  
  # Calculate total counts and dates
  result <- result %>%
    dplyr::mutate(
      total_code_count = rowSums(dplyr::select(., dplyr::all_of(count_cols)), na.rm = TRUE),
      first_code_date = do.call(pmin, c(dplyr::select(., dplyr::ends_with("_first")), na.rm = TRUE)),
      last_code_date = do.call(pmax, c(dplyr::select(., dplyr::ends_with("_last")), na.rm = TRUE)),
      healthcare_span_days = as.numeric(difftime(.data$last_code_date, .data$first_code_date, units = "days"))
    ) %>%
    dplyr::mutate(
      healthcare_span_days = ifelse(is.na(.data$healthcare_span_days), 0, .data$healthcare_span_days)
    )
  
  return(result)
}

#' Create Empty Utilization Tibble
#'
#' Creates an empty tibble with the correct structure for utilization data.
#'
#' @return Empty tibble with utilization columns
#'
#' @keywords internal
create_empty_utilization_tibble <- function() {
  tibble::tibble(
    person_id = integer(0),
    total_code_count = integer(0),
    condition_count = integer(0),
    procedure_count = integer(0),
    drug_count = integer(0),
    measurement_count = integer(0),
    observation_count = integer(0),
    first_code_date = as.Date(character(0)),
    last_code_date = as.Date(character(0)),
    healthcare_span_days = numeric(0)
  )
}

#' Extract Domain from Table Name
#'
#' Extracts domain name from OMOP table name.
#'
#' @param table_name OMOP table name
#'
#' @return Domain name
#'
#' @keywords internal
extract_domain_from_table <- function(table_name) {
  switch(table_name,
    "condition_occurrence" = "condition",
    "procedure_occurrence" = "procedure",
    "drug_exposure" = "drug",
    "measurement" = "measurement",
    "observation" = "observation",
    "visit_occurrence" = "visit",
    table_name  # fallback
  )
}

#' Validate Binomial Data Quality
#'
#' Performs comprehensive validation of data prepared for binomial mixture model.
#'
#' @param pheprob_data Tibble with S, C, and other columns
#' @param concept_ids Original concept IDs used
#'
#' @return List with validation results and recommendations
#'
#' @export
validate_binomial_data_quality <- function(pheprob_data, concept_ids = NULL) {
  
  validation_results <- list()
  
  # Basic data structure checks
  required_columns <- c("person_id", "S", "C")
  missing_columns <- setdiff(required_columns, colnames(pheprob_data))
  
  if (length(missing_columns) > 0) {
    validation_results$errors <- paste("Missing required columns:", paste(missing_columns, collapse = ", "))
    return(validation_results)
  }
  
  # Constraint validation: S <= C
  constraint_violations <- sum(pheprob_data$S > pheprob_data$C, na.rm = TRUE)
  validation_results$constraint_violations <- constraint_violations
  
  # Data quality metrics
  validation_results$summary_stats <- list(
    n_patients = nrow(pheprob_data),
    total_codes_range = range(pheprob_data$C, na.rm = TRUE),
    relevant_codes_range = range(pheprob_data$S, na.rm = TRUE),
    mean_success_rate = mean(pheprob_data$S / pmax(pheprob_data$C, 1), na.rm = TRUE),
    zero_total_codes = sum(pheprob_data$C == 0, na.rm = TRUE),
    zero_relevant_codes = sum(pheprob_data$S == 0, na.rm = TRUE)
  )
  
  # Distribution assessment
  success_rates <- pheprob_data$S / pmax(pheprob_data$C, 1)
  validation_results$distribution_stats <- list(
    success_rate_mean = mean(success_rates, na.rm = TRUE),
    success_rate_sd = sd(success_rates, na.rm = TRUE),
    success_rate_skew = assess_skewness(success_rates),
    bimodality_test = assess_bimodality(success_rates)
  )
  
  # Warnings and recommendations
  warnings <- character(0)
  recommendations <- character(0)
  
  if (constraint_violations > 0) {
    warnings <- c(warnings, glue::glue("{constraint_violations} patients have S > C"))
    recommendations <- c(recommendations, "Review concept definitions and data extraction logic")
  }
  
  if (validation_results$summary_stats$mean_success_rate < 0.01) {
    warnings <- c(warnings, "Very low disease-relevant code rate")
    recommendations <- c(recommendations, "Consider expanding concept definitions or checking phenotype validity")
  }
  
  if (validation_results$summary_stats$mean_success_rate > 0.5) {
    warnings <- c(warnings, "Very high disease-relevant code rate")
    recommendations <- c(recommendations, "Consider restricting concept definitions or checking for data quality issues")
  }
  
  if (validation_results$summary_stats$zero_total_codes > nrow(pheprob_data) * 0.1) {
    warnings <- c(warnings, "Many patients with zero total codes")
    recommendations <- c(recommendations, "Review inclusion criteria and data availability")
  }
  
  validation_results$warnings <- warnings
  validation_results$recommendations <- recommendations
  validation_results$overall_quality <- assess_overall_quality(validation_results)
  
  return(validation_results)
}

#' Assess Skewness
#'
#' Simple skewness assessment for distribution.
#'
#' @param x Numeric vector
#'
#' @return Skewness measure
#'
#' @keywords internal
assess_skewness <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3) return(NA)
  
  m3 <- mean((x - mean(x))^3)
  s3 <- sd(x)^3
  skewness <- m3 / s3
  
  return(skewness)
}

#' Assess Bimodality
#'
#' Simple test for bimodal distribution which would be expected for 
#' case/control mixture.
#'
#' @param x Numeric vector
#'
#' @return Bimodality coefficient
#'
#' @keywords internal
assess_bimodality <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 10) return(NA)
  
  # Simple bimodality coefficient based on skewness and kurtosis
  skew <- assess_skewness(x)
  kurt <- assess_kurtosis(x)
  
  # Bimodality coefficient (Pfister et al. 2013)
  bc <- (skew^2 + 1) / (kurt + 3 * (n-1)^2 / ((n-2)*(n-3)))
  
  return(bc)
}

#' Assess Kurtosis
#'
#' Simple kurtosis calculation.
#'
#' @param x Numeric vector
#'
#' @return Kurtosis measure
#'
#' @keywords internal
assess_kurtosis <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 4) return(NA)
  
  m4 <- mean((x - mean(x))^4)
  s4 <- sd(x)^4
  kurtosis <- m4 / s4 - 3  # Excess kurtosis
  
  return(kurtosis)
}

#' Assess Overall Data Quality
#'
#' Provides an overall quality score for the binomial mixture data.
#'
#' @param validation_results Results from validate_binomial_data_quality
#'
#' @return Quality score and interpretation
#'
#' @keywords internal
assess_overall_quality <- function(validation_results) {
  
  score <- 100  # Start with perfect score
  
  # Deduct for constraint violations
  if (validation_results$constraint_violations > 0) {
    violation_rate <- validation_results$constraint_violations / validation_results$summary_stats$n_patients
    score <- score - (violation_rate * 50)  # Up to 50 point deduction
  }
  
  # Deduct for extreme success rates
  mean_rate <- validation_results$summary_stats$mean_success_rate
  if (mean_rate < 0.01 || mean_rate > 0.5) {
    score <- score - 20
  }
  
  # Deduct for too many zero counts
  zero_rate <- validation_results$summary_stats$zero_total_codes / validation_results$summary_stats$n_patients
  if (zero_rate > 0.1) {
    score <- score - (zero_rate * 30)
  }
  
  # Bonus for good sample size
  if (validation_results$summary_stats$n_patients >= 1000) {
    score <- score + 5
  }
  
  score <- max(0, min(100, score))  # Clamp to [0, 100]
  
  interpretation <- dplyr::case_when(
    score >= 90 ~ "Excellent",
    score >= 80 ~ "Good", 
    score >= 70 ~ "Acceptable",
    score >= 60 ~ "Poor",
    TRUE ~ "Inadequate"
  )
  
  return(list(
    score = score,
    interpretation = interpretation,
    recommendation = if (score < 70) "Review data quality before proceeding" else "Data quality suitable for analysis"
  ))
}
