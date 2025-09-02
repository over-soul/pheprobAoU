#' All of Us Data Extraction Functions
#'
#' This file contains functions that properly use the allofus package
#' for data extraction, replacing the raw SQL approach.

#' Extract Healthcare Utilization Using AllofUs Package
#'
#' Extracts total healthcare utilization and disease-relevant counts using
#' proper allofus package functions and dbplyr remote computation.
#'
#' @param concept_ids A numeric vector of disease-relevant OMOP concept IDs
#' @param person_ids A numeric vector of person IDs. If NULL, includes all available persons.
#' @param domains A character vector specifying which OMOP domains to include.
#'   Default: c("condition_occurrence", "procedure_occurrence", "drug_exposure", "measurement", "observation")
#' @param date_range A list with 'start' and 'end' dates (Date objects) to limit
#'   the temporal scope of data extraction. If NULL, no date filtering is applied.
#' @param expand_concepts Logical indicating whether to expand concept hierarchies using concept_ancestor
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
  
  cli::cli_alert_info("Extracting AllofUs data for {length(concept_ids)} disease-relevant concepts")
  
  # Get connection - this function is only called when we're already in All of Us environment
  con <- tryCatch({
    connection <- allofus::aou_connect()
    if (is.null(connection)) {
      stop("Connection returned NULL")
    }
    connection
  }, error = function(e) {
    cli::cli_abort("Failed to connect to All of Us database: {e$message}")
  })
  
  # Define domain column mappings
  domain_cols <- list(
    condition_occurrence = list(tbl = "condition_occurrence",
                               concept = "condition_concept_id",
                               date = "condition_start_date"),
    procedure_occurrence = list(tbl = "procedure_occurrence",
                               concept = "procedure_concept_id",
                               date = "procedure_date"),
    drug_exposure = list(tbl = "drug_exposure",
                        concept = "drug_concept_id",
                        date = "drug_exposure_start_date"),
    measurement = list(tbl = "measurement",
                      concept = "measurement_concept_id",
                      date = "measurement_date"),
    observation = list(tbl = "observation",
                      concept = "observation_concept_id",
                      date = "observation_date"),
    device_exposure = list(tbl = "device_exposure",
                          concept = "device_concept_id",
                          date = "device_exposure_start_date")
  )
  
  # Filter to valid domains
  valid_domains <- intersect(domains, names(domain_cols))
  if (length(valid_domains) == 0) {
    cli::cli_abort("No valid domains specified. Valid options: {paste(names(domain_cols), collapse = ', ')}")
  }
  
  cli::cli_alert_info("Using domains: {paste(valid_domains, collapse = ', ')}")
  
  # Step 1: Create union of all events across domains
  cli::cli_progress_step("Creating cross-domain event union...")
  
  tbl_list <- purrr::map(valid_domains, function(d) {
    info <- domain_cols[[d]]
    
    tbl_events <- dplyr::tbl(con, info$tbl) %>%
      dplyr::transmute(
        person_id,
        concept_id = !!rlang::sym(info$concept),
        event_date = as.Date(!!rlang::sym(info$date)),
        domain = d
      ) %>%
      dplyr::filter(!is.na(person_id), concept_id > 0L)
    
    # Apply date filter if specified
    if (!is.null(date_range)) {
      if (!is.null(date_range$start)) {
        tbl_events <- tbl_events %>% dplyr::filter(event_date >= !!date_range$start)
      }
      if (!is.null(date_range$end)) {
        tbl_events <- tbl_events %>% dplyr::filter(event_date <= !!date_range$end)
      }
    }
    
    # Apply person filter if specified
    if (!is.null(person_ids) && length(person_ids) > 0) {
      tbl_events <- tbl_events %>% dplyr::filter(person_id %in% !!person_ids)
    }
    
    return(tbl_events)
  })
  
  events_union <- purrr::reduce(tbl_list, dplyr::union_all)
  
  # Step 2: Calculate total healthcare utilization (C)
  cli::cli_progress_step("Calculating total healthcare utilization (C)...")
  
  C_counts <- events_union %>%
    dplyr::count(person_id, name = "C")
  
  # Step 3: Expand disease concepts and calculate relevant counts (S)
  cli::cli_progress_step("Expanding disease concepts and calculating relevant counts (S)...")
  
  if (expand_concepts) {
    # Expand to descendants using concept_ancestor with IN clause
    disease_cs <- dplyr::tbl(con, "concept_ancestor") %>%
      dplyr::filter(ancestor_concept_id %in% !!as.integer(concept_ids)) %>%
      dplyr::transmute(concept_id = descendant_concept_id) %>%
      dplyr::distinct()
    
    # Include seed concepts themselves
    seeds_self <- dplyr::tbl(con, "concept") %>%
      dplyr::filter(concept_id %in% !!as.integer(concept_ids)) %>%
      dplyr::select(concept_id) %>%
      dplyr::distinct()
    
    disease_cs <- dplyr::union_all(disease_cs, seeds_self) %>% dplyr::distinct()
    
  } else {
    # Use only exact concept matches with IN clause
    disease_cs <- dplyr::tbl(con, "concept") %>%
      dplyr::filter(concept_id %in% !!as.integer(concept_ids)) %>%
      dplyr::select(concept_id) %>%
      dplyr::distinct()
  }
  
  # Calculate disease-relevant counts (S)
  S_counts <- events_union %>%
    dplyr::inner_join(disease_cs, by = "concept_id") %>%
    dplyr::count(person_id, name = "S")
  
  # Step 4: Combine into final dataset
  cli::cli_progress_step("Combining data for PheProb analysis...")
  
  # Start with person table or create person list from events
  if (!is.null(person_ids) && length(person_ids) > 0) {
    person_base <- dplyr::tbl(con, "person") %>%
      dplyr::filter(person_id %in% !!as.integer(person_ids)) %>%
      dplyr::select(person_id)
  } else {
    person_base <- dplyr::tbl(con, "person") %>%
      dplyr::select(person_id)
    
    # Apply max_persons limit if specified
    if (!is.null(max_persons) && is.numeric(max_persons) && max_persons > 0) {
      person_base <- person_base %>% 
        dplyr::slice_head(n = as.integer(max_persons))
    }
  }
  
  # Join everything together
  pheprob_data <- person_base %>%
    dplyr::left_join(C_counts, by = "person_id") %>%
    dplyr::left_join(S_counts, by = "person_id") %>%
    dplyr::mutate(
      C = dplyr::coalesce(C, 0L),
      S = dplyr::coalesce(S, 0L),
      success_rate = dplyr::case_when(
        C > 0 ~ S / C,
        TRUE ~ 0.0
      )
    ) %>%
    dplyr::filter(C > 0)  # Only include persons with some healthcare utilization
  
  # Collect the results
  cli::cli_progress_step("Collecting results...")
  result <- pheprob_data %>% dplyr::collect()
  
  if (nrow(result) == 0) {
    cli::cli_abort("No healthcare utilization data found")
  }
  
  cli::cli_alert_success("Extracted data for {nrow(result)} persons")
  cli::cli_alert_info("Total codes (C): {min(result$C)} - {max(result$C)}")
  cli::cli_alert_info("Relevant codes (S): {min(result$S)} - {max(result$S)}")
  
  return(result)
}

#' Replace Original Data Extraction Function
#' 
#' This function replaces the original prepare_pheprob_binomial_data function
#' to use the proper allofus approach.
#'
#' @inheritParams extract_allofus_pheprob_data
#' @return Tibble formatted for binomial mixture model
#' @keywords internal
prepare_pheprob_binomial_data_allofus <- function(concept_ids,
                                                  person_ids = NULL,
                                                  domains = c("condition_occurrence", "procedure_occurrence", 
                                                            "drug_exposure", "measurement", "observation"),
                                                  date_range = NULL,
                                                  exclude_concepts = NULL,
                                                  max_persons = NULL) {
  
  # Extract the basic (S, C) data
  pheprob_data <- extract_allofus_pheprob_data(
    concept_ids = concept_ids,
    person_ids = person_ids,
    domains = domains,
    date_range = date_range,
    expand_concepts = TRUE,
    max_persons = max_persons
  )
  
  # Add additional columns needed by the binomial mixture model
  result <- pheprob_data %>%
    dplyr::mutate(
      first_code_date = as.Date(NA),  # Would need additional logic to compute
      last_code_date = as.Date(NA),   # Would need additional logic to compute
      healthcare_span_days = as.numeric(NA)  # Would need additional logic to compute
    )
  
  return(result)
}
