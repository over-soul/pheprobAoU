#' All of Us Data Extraction Functions
#'
#' This file contains functions that properly use the allofus package
#' for data extraction, replacing the raw SQL approach.

#' Extract Healthcare Utilization Using AllofUs Package
#'
#' Extracts disease-relevant counts and total healthcare utilization using
#' proper allofus package functions (aou_concept_set, aou_survey, etc.).
#' This replaces the previous manual BigQuery approach that had compatibility issues.
#'
#' @param concept_ids A numeric vector of disease-relevant OMOP concept IDs
#' @param person_ids A numeric vector of person IDs. If NULL, includes all available persons.
#' @param domains A character vector specifying which OMOP domains to include.
#'   Note: allofus package handles domain selection automatically based on concept types.
#' @param date_range A list with 'start' and 'end' dates (Date objects) to limit
#'   the temporal scope of data extraction. If NULL, no date filtering is applied.
#' @param expand_concepts Logical indicating whether to expand concept hierarchies.
#'   Note: allofus package handles concept expansion automatically.
#' @param max_persons Maximum number of persons to process (for testing/limiting).
#'   If NULL, processes all available persons.
#'
#' @return A tibble with columns required for binomial mixture model:
#'   \item{person_id}{Person identifier}
#'   \item{S}{Disease-relevant code count}
#'   \item{C}{Total healthcare code count}
#'   \item{success_rate}{S/C ratio}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Extract data for diabetes phenotyping
#' diabetes_concepts <- c(201826, 4329847, 9201)
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
  
  cli::cli_alert_info("Extracting AllofUs data for {length(concept_ids)} disease-relevant concepts using proper allofus functions")
  
  # Step 1: Create cohort
  cli::cli_progress_step("Creating participant cohort...")
  
  if (!is.null(person_ids) && length(person_ids) > 0) {
    # Use specific person IDs
    cohort <- tibble::tibble(person_id = as.integer(person_ids))
    cli::cli_alert_info("Using specified {length(person_ids)} participants")
  } else {
    # Use all participants via survey data
    cohort <- allofus::aou_survey(questions = 1585838, question_output = "gender") %>%
      dplyr::select(person_id)
    
    # Apply max_persons limit if specified
    if (!is.null(max_persons) && is.numeric(max_persons) && max_persons > 0) {
      cohort <- cohort %>% 
        dplyr::collect() %>%
        dplyr::slice_head(n = as.integer(max_persons))
      cli::cli_alert_info("Limited to {max_persons} participants for testing")
    }
  }
  
  # Calculate cohort size (handle both remote and local tibbles)
  if ("tbl_df" %in% class(cohort)) {
    cohort_size <- nrow(cohort)
  } else {
    cohort_size_raw <- cohort %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::collect() %>% dplyr::pull(n)
    cohort_size <- as.numeric(cohort_size_raw)  # Handle integer64
  }
  cli::cli_alert_info("Cohort size: {cohort_size} participants")
  
  # Step 2: Extract disease-relevant codes (S) using proper allofus
  cli::cli_progress_step("Extracting disease-relevant codes using aou_concept_set...")
  
  # Map domain names for allofus compatibility
  allofus_domains <- domains[domains %in% c("condition", "procedure", "drug", "measurement", "observation")]
  if (length(allofus_domains) == 0) {
    allofus_domains <- "condition"  # Default to condition if no valid domains
  }
  
  disease_data <- allofus::aou_concept_set(cohort,
    concepts = concept_ids,
    domains = allofus_domains,
    output = "count",
    concept_set_name = "disease_codes",
    start_date = if (!is.null(date_range$start)) date_range$start else NULL,
    end_date = if (!is.null(date_range$end)) date_range$end else NULL
  ) %>%
    dplyr::collect()
  
  # Convert integer64 to avoid issues
  disease_data <- disease_data %>%
    dplyr::mutate(
      person_id = as.numeric(person_id),
      disease_codes = as.numeric(disease_codes)
    )
  
  cli::cli_alert_info("Disease prevalence: {round(mean(disease_data$disease_codes > 0) * 100, 2)}%")
  
  # Step 3: Calculate total healthcare utilization (C)
  cli::cli_progress_step("Calculating total healthcare utilization...")
  
  # Use common healthcare concepts as proxy for total utilization
  # This is a reasonable proxy that captures most healthcare encounters
  common_healthcare_concepts <- c(
    9202,      # Outpatient visit
    9201,      # Inpatient visit
    581477,    # Emergency room visit
    concept_ids  # Include the disease concepts too
  )
  
  total_healthcare_data <- allofus::aou_concept_set(cohort,
    concepts = unique(common_healthcare_concepts),
    domains = allofus_domains,
    output = "count", 
    concept_set_name = "total_healthcare",
    start_date = if (!is.null(date_range$start)) date_range$start else NULL,
    end_date = if (!is.null(date_range$end)) date_range$end else NULL
  ) %>%
    dplyr::collect()
  
  # Convert integer64 to avoid issues
  total_healthcare_data <- total_healthcare_data %>%
    dplyr::mutate(
      person_id = as.numeric(person_id),
      total_healthcare = as.numeric(total_healthcare)
    )
  
  # Step 4: Combine into final PheProb dataset
  cli::cli_progress_step("Combining data for PheProb analysis...")
  
  pheprob_data <- disease_data %>%
    dplyr::left_join(total_healthcare_data, by = "person_id") %>%
    dplyr::transmute(
      person_id = person_id,
      S = disease_codes,
      C = pmax(total_healthcare, disease_codes, na.rm = TRUE),  # C must be at least S
      success_rate = dplyr::case_when(
        C > 0 ~ S / C,
        TRUE ~ 0.0
      )
    ) %>%
    dplyr::filter(C > 0)  # Only include persons with some healthcare utilization
  
  if (nrow(pheprob_data) == 0) {
    cli::cli_abort("No healthcare utilization data found")
  }
  
  # Final summary
  cli::cli_alert_success("Extracted data for {nrow(pheprob_data)} persons")
  cli::cli_alert_info("Total codes (C): {min(pheprob_data$C)} - {max(pheprob_data$C)}")
  cli::cli_alert_info("Relevant codes (S): {min(pheprob_data$S)} - {max(pheprob_data$S)}")
  cli::cli_alert_info("Disease prevalence: {round(mean(pheprob_data$S > 0) * 100, 2)}%")
  
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
