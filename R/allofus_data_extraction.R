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
#' @param con Optional database connection from allofus::aou_connect(). If NULL,
#'   a new connection will be created.
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
                                         max_persons = NULL,
                                         con = NULL) {
  
  # Validate inputs
  if (!is.numeric(concept_ids) || length(concept_ids) == 0) {
    cli::cli_abort("concept_ids must be a non-empty numeric vector")
  }
  
  # Handle connection - use provided connection or create new one
  connection_provided <- !is.null(con)
  if (!connection_provided) {
    # Ensure allofus package is available
    if (!requireNamespace("allofus", quietly = TRUE)) {
      cli::cli_abort("allofus package is required but not available")
    }
    con <- allofus::aou_connect()
  }
  
  cli::cli_alert_info("Extracting AllofUs data for {length(concept_ids)} disease-relevant concepts using {if(connection_provided) 'shared' else 'new'} connection")
  
  # Call the version that accepts connection
  return(extract_allofus_pheprob_data_with_connection(
    con = con,
    concept_ids = concept_ids,
    person_ids = person_ids,
    domains = domains,
    date_range = date_range,
    expand_concepts = expand_concepts,
    max_persons = max_persons
  ))
}

#' Extract AllofUs PheProb Data with Existing Connection
#'
#' Version of extract_allofus_pheprob_data that accepts an existing database connection
#' to avoid multiple connection overhead.
#'
#' @param con Database connection object from allofus::aou_connect()
#' @param concept_ids A numeric vector of disease-relevant OMOP concept IDs
#' @param person_ids A numeric vector of person IDs. If NULL, uses general population.
#' @param domains A character vector specifying which OMOP domains to include.
#' @param date_range A list with 'start' and 'end' dates (Date objects) to limit
#'   the temporal scope of data extraction. If NULL, no date filtering is applied.
#' @param expand_concepts Logical indicating whether to expand concept hierarchies
#'   using concept_ancestor table (default: TRUE).
#' @param max_persons Maximum number of persons to process (for testing/limiting).
#'   If NULL, processes all available persons.
#'
#' @return A tibble with columns required for binomial mixture model
#'
#' @keywords internal
extract_allofus_pheprob_data_with_connection <- function(con,
                                                        concept_ids,
                                                        person_ids = NULL,
                                                        domains = c("condition"),
                                                        date_range = NULL,
                                                        expand_concepts = TRUE,
                                                        max_persons = NULL) {
  
  # Validate inputs
  if (!is.numeric(concept_ids) || length(concept_ids) == 0) {
    cli::cli_abort("concept_ids must be a non-empty numeric vector")
  }
  
  cli::cli_alert_info("Extracting AllofUs data for {length(concept_ids)} disease-relevant concepts using shared connection")
  
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
  
  pheprob_data <- cohort %>%
    dplyr::left_join(total_utilization, by = "person_id") %>%
    dplyr::left_join(disease_codes, by = "person_id") %>%
    dplyr::mutate(
      total_condition_codes = dplyr::coalesce(total_condition_codes, 0L),
      disease_codes = dplyr::coalesce(disease_codes, 0L)
    ) %>%
    dplyr::filter(total_condition_codes > 0) %>%  # Remove patients with no healthcare utilization
    dplyr::mutate(
      C = total_condition_codes,
      S = disease_codes,
      success_rate = S / C
    ) %>%
    dplyr::select(person_id, S, C, success_rate) %>%
    dplyr::collect()
  
  # Remove patients with zero total codes (no healthcare utilization)
  pheprob_data <- pheprob_data %>%
    dplyr::filter(C > 0)
  
  cli::cli_alert_success("Extracted data for {nrow(pheprob_data)} patients with healthcare utilization")
  
  if (nrow(pheprob_data) > 0) {
    disease_prevalence <- mean(pheprob_data$S > 0)
    mean_success_rate <- mean(pheprob_data$success_rate)
    
    cli::cli_alert_info("Disease prevalence: {round(disease_prevalence * 100, 1)}%")
    cli::cli_alert_info("Mean success rate: {round(mean_success_rate, 3)}")
    cli::cli_alert_info("C range: {min(pheprob_data$C)} - {max(pheprob_data$C)}")
    cli::cli_alert_info("S range: {min(pheprob_data$S)} - {max(pheprob_data$S)}")
  }
  
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


#' View Expanded Disease Concepts from Hierarchy
#'
#' Displays what concepts are included when concept hierarchy expansion is applied
#' to the provided input concepts. This function helps users understand what
#' additional concepts are included in their phenotype definitions through the
#' OMOP concept_ancestor relationships.
#'
#' @param concept_ids A numeric vector of OMOP concept IDs to expand
#' @param include_names Logical indicating whether to include concept names and 
#'   additional metadata (default: TRUE)
#' @param include_original Logical indicating whether to include the original 
#'   concepts in the output (default: TRUE)
#' @param max_concepts Maximum number of concepts to return (default: NULL for no limit)
#'
#' @return A tibble with expanded concept information:
#'   \item{concept_id}{OMOP concept identifier (descendant)}
#'   \item{concept_name}{Human-readable concept name (if include_names = TRUE)}
#'   \item{domain_id}{OMOP domain (if include_names = TRUE)}
#'   \item{vocabulary_id}{OMOP vocabulary (if include_names = TRUE)}
#'   \item{concept_class_id}{OMOP concept class (if include_names = TRUE)}
#'   \item{is_original}{Whether this was one of the input concepts (if include_original = TRUE)}
#'
#' @details
#' This function uses the same concept expansion logic as the main phenotyping
#' functions in pheprobAoU. It queries the concept_ancestor table to find all
#' descendant concepts for the provided concept IDs, giving users visibility
#' into what concepts are actually included in their analyses.
#' 
#' The expansion includes:
#' - All descendant concepts from the OMOP concept hierarchy
#' - Deduplication of concept mappings
#' - Optional inclusion of concept names and metadata
#' - Optional filtering to manageable result sizes
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # View expanded diabetes concepts
#' diabetes_concepts <- c(201820, 201826, 4193704)
#' expanded <- view_expanded_concepts(diabetes_concepts)
#' print(expanded)
#' 
#' # View just concept IDs without names (faster)
#' expanded_ids_only <- view_expanded_concepts(
#'   diabetes_concepts, 
#'   include_names = FALSE
#' )
#' 
#' # Limit results to first 100 concepts
#' expanded_limited <- view_expanded_concepts(
#'   diabetes_concepts,
#'   max_concepts = 100
#' )
#' 
#' # Combine options: fast and limited for large concept sets
#' fast_limited <- view_expanded_concepts(
#'   diabetes_concepts,
#'   include_names = FALSE,
#'   max_concepts = 10
#' )
#' }
view_expanded_concepts <- function(concept_ids,
                                   include_names = TRUE,
                                   include_original = TRUE, 
                                   max_concepts = NULL) {
  
  # Input validation
  if (!is.numeric(concept_ids) || length(concept_ids) == 0) {
    cli::cli_abort("concept_ids must be a non-empty numeric vector")
  }
  
  if (!is.logical(include_names) || length(include_names) != 1) {
    cli::cli_abort("include_names must be a single logical value")
  }
  
  if (!is.logical(include_original) || length(include_original) != 1) {
    cli::cli_abort("include_original must be a single logical value")
  }
  
  if (!is.null(max_concepts) && (!is.numeric(max_concepts) || length(max_concepts) != 1 || max_concepts <= 0)) {
    cli::cli_abort("max_concepts must be NULL or a positive number")
  }
  
  # Ensure allofus package is available
  if (!requireNamespace("allofus", quietly = TRUE)) {
    cli::cli_abort("allofus package is required but not available")
  }
  
  cli::cli_alert_info("Expanding {length(concept_ids)} input concept{?s} using OMOP hierarchy...")
  
  tryCatch({
    # Connect to database
    con <- allofus::aou_connect()
    
    concept_ids_integer <- as.integer(concept_ids)
    
    # Get expanded concepts using concept hierarchy
    concept_ancestor <- dplyr::tbl(con, "concept_ancestor")
    
    expanded_concepts <- concept_ancestor %>%
      dplyr::filter(ancestor_concept_id %in% !!concept_ids_integer) %>%
      dplyr::distinct(descendant_concept_id) %>%
      dplyr::collect()
    
    cli::cli_alert_info("Found {nrow(expanded_concepts)} expanded concept{?s}")
    
    # Optionally include original concepts
    if (include_original) {
      # Add original concepts that might not have descendants
      original_concepts <- tibble::tibble(descendant_concept_id = concept_ids_integer)
      expanded_concepts <- dplyr::bind_rows(expanded_concepts, original_concepts) %>%
        dplyr::distinct(descendant_concept_id)
      
      cli::cli_alert_info("Including {length(concept_ids)} original concept{?s} in output")
    }
    
    # Prepare basic result
    result <- expanded_concepts %>%
      dplyr::rename(concept_id = descendant_concept_id)
    
    # Optionally add concept names and metadata
    if (include_names && nrow(result) > 0) {
      cli::cli_progress_step("Retrieving concept names and metadata...")
      
      concept_info <- dplyr::tbl(con, "concept") %>%
        dplyr::filter(concept_id %in% !!result$concept_id) %>%
        dplyr::select(concept_id, concept_name, domain_id, vocabulary_id, concept_class_id) %>%
        dplyr::collect()
      
      result <- result %>%
        dplyr::left_join(concept_info, by = "concept_id") %>%
        dplyr::mutate(
          concept_name = ifelse(is.na(.data$concept_name), 
                               paste("Unknown concept", .data$concept_id), 
                               .data$concept_name)
        )
    }
    
    # Add is_original flag if requested
    if (include_original) {
      result <- result %>%
        dplyr::mutate(is_original = .data$concept_id %in% concept_ids_integer)
    }
    
    # Arrange by concept name if available, otherwise by concept_id
    if (include_names && "concept_name" %in% colnames(result)) {
      result <- result %>% dplyr::arrange(.data$concept_name)
    } else {
      result <- result %>% dplyr::arrange(.data$concept_id)
    }
    
    # Apply limit if specified (after all processing to get correct final count)
    if (!is.null(max_concepts) && nrow(result) > max_concepts) {
      result <- result %>%
        dplyr::slice_head(n = max_concepts)
      cli::cli_alert_warning("Limited output to first {max_concepts} concept{?s}")
    }
    
    cli::cli_alert_success("Successfully retrieved expanded concept information")
    
    return(result)
    
  }, error = function(e) {
    cli::cli_abort("Failed to expand concepts: {e$message}")
  })
}
