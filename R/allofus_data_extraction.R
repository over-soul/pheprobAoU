#' All of Us Data Extraction Functions
#'
#' This file contains functions that properly use the allofus package
#' for data extraction, replacing the raw SQL approach.

#' Extract Healthcare Utilization Using Direct SQL Approach
#'
#' Extracts disease-relevant counts and total healthcare utilization using
#' direct SQL queries with concept hierarchy expansion and proper healthcare utilization.
#' This approach fixes critical issues with the previous aou_concept_set method.
#'
#' @param concept_ids A numeric vector of disease-relevant OMOP concept IDs
#' @param person_ids A numeric vector of person IDs. If NULL, uses general population.
#' @param domains A character vector specifying which OMOP domains to include.
#'   Currently focuses on condition_occurrence domain for optimal performance.
#' @param date_range A list with 'start' and 'end' dates (Date objects) to limit
#'   the temporal scope of data extraction. If NULL, no date filtering is applied.
#' @param expand_concepts Logical indicating whether to expand concept hierarchies
#'   using concept_ancestor table (default: TRUE).
#' @param max_persons Maximum number of persons to process (for testing/limiting).
#'   If NULL, processes all available persons.
#'
#' @return A tibble with columns required for binomial mixture model:
#'   \item{person_id}{Person identifier}
#'   \item{S}{Disease-relevant code count (using concept hierarchy)}
#'   \item{C}{Total healthcare code count (source-coded conditions)}
#'   \item{success_rate}{S/C ratio}
#'
#' @details
#' This function uses direct SQL queries to extract:
#' - S: Disease-relevant billing code count (using concept hierarchy)
#' - C: Total healthcare utilization (source-coded conditions) 
#' - Proper case/control mixture from general population
#' 
#' Key improvements over previous approach:
#' - Uses concept_ancestor for comprehensive phenotype expansion
#' - Counts source-coded conditions for realistic healthcare utilization
#' - Uses general population instead of survey-biased cohorts
#' - Handles integer64 data types properly
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Extract data for diabetes phenotyping
#' diabetes_concepts <- c(201820, 201826, 4193704)
#' pheprob_data <- extract_allofus_pheprob_data(diabetes_concepts)
#' }
extract_allofus_pheprob_data <- function(concept_ids,
                                         person_ids = NULL,
                                         domains = c("condition_occurrence", "procedure_occurrence", 
                                                   "drug_exposure", "measurement", "observation"),
                                         date_range = NULL,
                                         expand_concepts = TRUE,
                                         max_persons = NULL) {
  
  # Validate inputs
  if (!is.numeric(concept_ids) || length(concept_ids) == 0) {
    cli::cli_abort("concept_ids must be a non-empty numeric vector")
  }
  
  # Ensure allofus package is available
  if (!requireNamespace("allofus", quietly = TRUE)) {
    cli::cli_abort("allofus package is required but not available")
  }
  
  cli::cli_alert_info("Extracting AllofUs data for {length(concept_ids)} disease-relevant concepts using direct SQL approach")
  
  # Connect to database
  con <- allofus::aou_connect()
  
  # Step 1: Create cohort
  cli::cli_progress_step("Creating participant cohort...")
  
  if (!is.null(person_ids) && length(person_ids) > 0) {
    # Use specific person IDs
    cohort <- dplyr::tbl(con, "person") %>%
      dplyr::filter(person_id %in% !!person_ids) %>%
      dplyr::distinct(person_id)
    cli::cli_alert_info("Using specified {length(person_ids)} participants")
  } else {
    # Use general population (not survey-based to avoid bias)
    cohort <- dplyr::tbl(con, "person") %>%
      dplyr::distinct(person_id)
    
    # Apply max_persons limit if specified
    if (!is.null(max_persons) && is.numeric(max_persons) && max_persons > 0) {
      cohort <- cohort %>% 
        dplyr::slice_head(n = as.integer(max_persons))
      cli::cli_alert_info("Limited to {max_persons} participants for testing")
    }
  }
  
  # Calculate cohort size
  cohort_size_raw <- cohort %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::collect() %>% dplyr::pull(n)
  cohort_size <- as.numeric(cohort_size_raw)  # Handle integer64
  cli::cli_alert_info("Cohort size: {as.character(cohort_size)} participants")
  
  # Step 2: Expand concepts using hierarchy if requested
  cli::cli_progress_step("Expanding disease concepts using hierarchy...")
  
  concept_ids_integer <- as.integer(concept_ids)
  
  if (expand_concepts) {
    # Use concept hierarchy for comprehensive phenotype definition
    concept_ancestor <- dplyr::tbl(con, "concept_ancestor")
    
    disease_concepts_expanded <- concept_ancestor %>%
      dplyr::filter(ancestor_concept_id %in% !!concept_ids_integer) %>%
      dplyr::distinct(descendant_concept_id) %>%  # Remove duplicate concept mappings
      dplyr::transmute(condition_concept_id = descendant_concept_id)
    
    expanded_count <- disease_concepts_expanded %>% 
      dplyr::summarise(n = dplyr::n()) %>% 
      dplyr::collect() %>% 
      dplyr::pull(n)
    cli::cli_alert_info("Expanded to {as.character(expanded_count)} related concepts")
  } else {
    # Use only the provided concepts (avoid temporary tables for BigQuery compatibility)
    disease_concepts_expanded <- dplyr::tbl(con, sql(
      paste0("SELECT UNNEST([", paste(concept_ids_integer, collapse = ", "), "]) as condition_concept_id")
    ))
    cli::cli_alert_info("Using {length(concept_ids)} provided concepts without expansion")
  }
  
  # Step 3: Calculate total healthcare utilization (C) - source-coded conditions
  cli::cli_progress_step("Calculating total healthcare utilization...")
  
  condition_occurrence <- dplyr::tbl(con, "condition_occurrence")
  
  # Build date filter if specified
  date_filter <- TRUE
  if (!is.null(date_range)) {
    if (!is.null(date_range$start)) {
      date_filter <- date_filter & condition_occurrence$condition_start_date >= !!date_range$start
    }
    if (!is.null(date_range$end)) {
      date_filter <- date_filter & condition_occurrence$condition_start_date <= !!date_range$end
    }
  }
  
  total_utilization <- condition_occurrence %>%
    dplyr::inner_join(cohort, by = "person_id") %>%
    dplyr::filter(!!date_filter) %>%
    dplyr::summarise(
      total_condition_codes = sum(dplyr::case_when(
        condition_source_concept_id != 0 ~ 1L,
        TRUE ~ 0L
      ), na.rm = TRUE),
      .by = person_id
    )
  
  # Step 4: Calculate disease-relevant codes (S)
  cli::cli_progress_step("Extracting disease-relevant codes...")
  
  disease_codes <- condition_occurrence %>%
    dplyr::inner_join(cohort, by = "person_id") %>%
    dplyr::inner_join(disease_concepts_expanded, by = "condition_concept_id") %>%
    dplyr::filter(!!date_filter) %>%
    dplyr::summarise(
      disease_codes = sum(dplyr::case_when(
        condition_source_concept_id != 0 ~ 1L,
        TRUE ~ 0L
      ), na.rm = TRUE),
      .by = person_id
    )
  
  # Step 5: Combine into final PheProb dataset
  cli::cli_progress_step("Combining data for PheProb analysis...")
  
  pheprob_data <- total_utilization %>%
    dplyr::left_join(disease_codes, by = "person_id") %>%
    dplyr::transmute(
      person_id = person_id,
      C = total_condition_codes,
      S = dplyr::coalesce(disease_codes, 0L),
      success_rate = dplyr::case_when(
        C > 0 ~ as.numeric(S) / as.numeric(C),
        TRUE ~ 0.0
      )
    ) %>%
    dplyr::filter(C > 0) %>%  # Only include persons with some healthcare utilization
    dplyr::collect()
  
  # Convert integer64 to numeric to avoid display issues
  pheprob_data <- pheprob_data %>%
    dplyr::mutate(
      person_id = as.numeric(person_id),
      S = as.numeric(S),
      C = as.numeric(C),
      success_rate = as.numeric(success_rate)
    )
  
  if (nrow(pheprob_data) == 0) {
    cli::cli_abort("No healthcare utilization data found")
  }
  
  # Step 6: Validate data constraints (should be clean with duplicate removal)
  cli::cli_progress_step("Validating data constraints...")
  
  # Check S <= C constraint (should be satisfied with DISTINCT fix)
  violations <- sum(pheprob_data$S > pheprob_data$C)
  if (violations > 0) {
    cli::cli_alert_warning("Found {violations} patients where S > C. This is unexpected with duplicate removal.")
    cli::cli_alert_info("Setting S = C for these cases as a safety measure.")
    
    # Fix any remaining constraint violations
    violation_indices <- which(pheprob_data$S > pheprob_data$C)
    pheprob_data$S[violation_indices] <- pheprob_data$C[violation_indices]
    
    # Recalculate success rate after fixing violations
    pheprob_data$success_rate <- dplyr::case_when(
      pheprob_data$C > 0 ~ pheprob_data$S / pheprob_data$C,
      TRUE ~ 0.0
    )
  } else {
    cli::cli_alert_success("Data constraints satisfied: S <= C for all patients")
  }
  
  # Final summary
  cli::cli_alert_success("Extracted data for {nrow(pheprob_data)} persons")
  cli::cli_alert_info("Total codes (C): {as.character(min(pheprob_data$C))} - {as.character(max(pheprob_data$C))}")
  cli::cli_alert_info("Relevant codes (S): {as.character(min(pheprob_data$S))} - {as.character(max(pheprob_data$S))}")
  cli::cli_alert_info("Disease prevalence: {as.character(round(mean(pheprob_data$S > 0) * 100, 2))}%")
  
  return(pheprob_data)
}

#' Prepare PheProb Data Using AllofUs Functions
#' 
#' This function replaces the original manual BigQuery approach
#' with proper allofus package functions.
#'
#' @inheritParams extract_allofus_pheprob_data
#' @param exclude_concepts Concepts to exclude (currently ignored)
#' @return Tibble formatted for binomial mixture model
#' @keywords internal
prepare_pheprob_binomial_data_allofus <- function(concept_ids,
                                                  person_ids = NULL,
                                                  domains = c("condition_occurrence", "procedure_occurrence", 
                                                            "drug_exposure", "measurement", "observation"),
                                                  date_range = NULL,
                                                  exclude_concepts = NULL,
                                                  max_persons = NULL) {
  
  # Extract the basic (S, C) data using proper allofus functions
  pheprob_data <- extract_allofus_pheprob_data(
    concept_ids = concept_ids,
    person_ids = person_ids,
    domains = domains,
    date_range = date_range,
    expand_concepts = TRUE,
    max_persons = max_persons
  )
  
  # Ensure all numeric columns are properly converted from integer64
  result <- pheprob_data %>%
    dplyr::mutate(
      person_id = as.numeric(person_id),
      S = as.numeric(S),
      C = as.numeric(C),
      success_rate = as.numeric(success_rate),
      # Add placeholders for additional columns (these would need more complex logic to compute accurately)
      first_code_date = as.Date(NA),
      last_code_date = as.Date(NA), 
      healthcare_span_days = as.numeric(NA)
    )
  
  return(result)
}
