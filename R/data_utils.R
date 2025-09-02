#' Extract EHR Features for PheProb Calculation
#'
#' Extracts relevant electronic health record features from All of Us database
#' for the specified concept IDs and person IDs.
#'
#' @param concept_ids A numeric vector of validated OMOP concept IDs
#' @param person_ids A numeric vector of validated person IDs. If NULL, extracts
#'   for all available persons.
#' @param domains A character vector specifying which OMOP domains to search.
#'   Default: c("condition", "procedure", "drug", "measurement", "observation")
#' @param date_range A list with 'start' and 'end' dates (Date objects) to limit
#'   the temporal scope of data extraction. If NULL, no date filtering is applied.
#' @param max_persons Maximum number of persons to process (for testing/limiting).
#'   If NULL, processes all available persons.
#'
#' @return A tibble with columns:
#'   \item{person_id}{Person identifier}
#'   \item{concept_id}{OMOP concept identifier}
#'   \item{concept_name}{Human-readable concept name}
#'   \item{domain_id}{OMOP domain (condition_occurrence, procedure_occurrence, etc.)}
#'   \item{occurrence_count}{Number of times concept appears for this person}
#'   \item{first_occurrence_date}{Date of first occurrence}
#'   \item{last_occurrence_date}{Date of last occurrence}
#'   \item{days_since_first}{Days between first and last occurrence}
#'   \item{total_records}{Total number of records for this person-concept pair}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Extract features for diabetes-related concepts
#' diabetes_concepts <- c(201826, 4329847)
#' features <- extract_ehr_features(diabetes_concepts)
#' head(features)
#' }
extract_ehr_features <- function(concept_ids, 
                                person_ids = NULL, 
                                domains = c("condition", "procedure", "drug", "measurement", "observation"),
                                date_range = NULL,
                                max_persons = NULL) {
  
  # Validate inputs
  if (length(concept_ids) == 0) {
    cli::cli_abort("concept_ids cannot be empty")
  }
  
  # Map domain names to OMOP tables
  domain_tables <- list(
    "condition" = "condition_occurrence",
    "procedure" = "procedure_occurrence", 
    "drug" = "drug_exposure",
    "measurement" = "measurement",
    "observation" = "observation"
  )
  
  # Filter to valid domains
  valid_domains <- intersect(domains, names(domain_tables))
  if (length(valid_domains) == 0) {
    cli::cli_abort("No valid domains specified. Valid options: {paste(names(domain_tables), collapse = ', ')}")
  }
  
  cli::cli_alert_info("Extracting EHR features from {length(valid_domains)} domain(s): {paste(valid_domains, collapse = ', ')}")
  
  # Initialize progress bar
  cli::cli_progress_bar("Extracting features", total = length(valid_domains))
  
  # Extract features from each domain
  all_features <- list()
  
  for (domain in valid_domains) {
    cli::cli_progress_update()
    
    table_name <- domain_tables[[domain]]
    features <- extract_domain_features(
      table_name = table_name,
      concept_ids = concept_ids,
      person_ids = person_ids,
      date_range = date_range,
      max_persons = max_persons
    )
    
    if (nrow(features) > 0) {
      features$domain_id <- domain
      all_features[[domain]] <- features
    }
  }
  
  cli::cli_progress_done()
  
  # Combine all features
  if (length(all_features) > 0) {
    combined_features <- dplyr::bind_rows(all_features)
    
    # Add concept names
    combined_features <- add_concept_names(combined_features)
    
    cli::cli_alert_success("Extracted {nrow(combined_features)} feature records for {length(unique(combined_features$person_id))} persons")
    
    return(combined_features)
  } else {
    cli::cli_alert_info("No features found for the specified criteria")
    return(tibble::tibble(
      person_id = numeric(0),
      concept_id = numeric(0),
      concept_name = character(0),
      domain_id = character(0),
      occurrence_count = numeric(0),
      first_occurrence_date = as.Date(character(0)),
      last_occurrence_date = as.Date(character(0)),
      days_since_first = numeric(0),
      total_records = numeric(0)
    ))
  }
}


#' Extract Features from Specific OMOP Domain
#'
#' Internal function to extract features from a specific OMOP domain table.
#'
#' @param table_name Name of the OMOP table to query
#' @param concept_ids Vector of concept IDs to search for
#' @param person_ids Vector of person IDs to limit to (optional)
#' @param date_range Date range for filtering (optional)
#' @param max_persons Maximum number of persons to include
#'
#' @return Tibble with extracted features
#'
#' @keywords internal
extract_domain_features <- function(table_name, concept_ids, person_ids, date_range, max_persons) {
  
  # Build SQL query based on table type
  concept_column <- get_concept_column(table_name)
  date_column <- get_date_column(table_name)
  
  # Base query
  base_query <- glue::glue("
    SELECT 
      person_id,
      {concept_column} as concept_id,
      {date_column} as occurrence_date,
      COUNT(*) OVER (PARTITION BY person_id, {concept_column}) as occurrence_count,
      MIN({date_column}) OVER (PARTITION BY person_id, {concept_column}) as first_occurrence_date,
      MAX({date_column}) OVER (PARTITION BY person_id, {concept_column}) as last_occurrence_date,
      ROW_NUMBER() OVER (PARTITION BY person_id, {concept_column} ORDER BY {date_column}) as rn
    FROM {table_name}
    WHERE {concept_column} IN ({paste(concept_ids, collapse = ', ')})
  ")
  
  # Add person ID filter if specified
  if (!is.null(person_ids) && length(person_ids) > 0) {
    person_filter <- paste(person_ids, collapse = ", ")
    base_query <- paste(base_query, "AND person_id IN (", person_filter, ")")
  }
  
  # Add date range filter if specified
  if (!is.null(date_range) && !is.null(date_range$start) && !is.null(date_range$end)) {
    base_query <- paste(base_query, 
                       "AND", date_column, "BETWEEN", 
                       paste0("'", date_range$start, "'"), "AND", 
                       paste0("'", date_range$end, "'"))
  }
  
  # Add person limit if specified
  if (!is.null(max_persons)) {
    base_query <- paste(base_query, "AND person_id IN (SELECT DISTINCT person_id FROM", table_name, "LIMIT", max_persons, ")")
  }
  
  # Wrap in a query to get unique person-concept combinations
  final_query <- glue::glue("
    WITH base_data AS ({base_query})
    SELECT DISTINCT
      person_id,
      concept_id,
      occurrence_count,
      first_occurrence_date,
      last_occurrence_date,
      CASE 
        WHEN first_occurrence_date = last_occurrence_date THEN 0
        ELSE DATE_DIFF(last_occurrence_date, first_occurrence_date, DAY)
      END as days_since_first,
      occurrence_count as total_records
    FROM base_data
    WHERE rn = 1
  ")
  
  # Execute query
  tryCatch({
    result <- allofus::aou_sql(final_query)
    
    if (nrow(result) > 0) {
      # Convert date columns
      result$first_occurrence_date <- as.Date(result$first_occurrence_date)
      result$last_occurrence_date <- as.Date(result$last_occurrence_date)
      
      return(tibble::as_tibble(result))
    } else {
      return(tibble::tibble(
        person_id = numeric(0),
        concept_id = numeric(0),
        occurrence_count = numeric(0),
        first_occurrence_date = as.Date(character(0)),
        last_occurrence_date = as.Date(character(0)),
        days_since_first = numeric(0),
        total_records = numeric(0)
      ))
    }
    
  }, error = function(e) {
    cli::cli_alert_info("Error querying {table_name}: {e$message}")
    return(tibble::tibble(
      person_id = numeric(0),
      concept_id = numeric(0),
      occurrence_count = numeric(0),
      first_occurrence_date = as.Date(character(0)),
      last_occurrence_date = as.Date(character(0)),
      days_since_first = numeric(0),
      total_records = numeric(0)
    ))
  })
}


#' Get Concept Column Name for OMOP Table
#'
#' Returns the appropriate concept ID column name for each OMOP table.
#'
#' @param table_name Name of the OMOP table
#'
#' @return Character string with column name
#'
#' @keywords internal
get_concept_column <- function(table_name) {
  concept_columns <- list(
    "condition_occurrence" = "condition_concept_id",
    "procedure_occurrence" = "procedure_concept_id",
    "drug_exposure" = "drug_concept_id",
    "measurement" = "measurement_concept_id",
    "observation" = "observation_concept_id"
  )
  
  return(concept_columns[[table_name]] %||% "concept_id")
}


#' Get Date Column Name for OMOP Table
#'
#' Returns the appropriate date column name for each OMOP table.
#'
#' @param table_name Name of the OMOP table
#'
#' @return Character string with column name
#'
#' @keywords internal
get_date_column <- function(table_name) {
  date_columns <- list(
    "condition_occurrence" = "condition_start_date",
    "procedure_occurrence" = "procedure_date",
    "drug_exposure" = "drug_exposure_start_date",
    "measurement" = "measurement_date",
    "observation" = "observation_date"
  )
  
  return(date_columns[[table_name]] %||% "occurrence_date")
}


#' Add Concept Names to Feature Data
#'
#' Joins concept names from the concept table to the extracted features.
#'
#' @param features_data Tibble with extracted features
#'
#' @return Tibble with concept names added
#'
#' @keywords internal
add_concept_names <- function(features_data) {
  
  if (nrow(features_data) == 0) {
    return(features_data)
  }
  
  unique_concept_ids <- unique(features_data$concept_id)
  
  tryCatch({
    concept_names <- allofus::aou_sql(
      glue::glue("
        SELECT concept_id, concept_name, vocabulary_id, concept_class_id
        FROM concept 
        WHERE concept_id IN ({paste(unique_concept_ids, collapse = ', ')})
      ")
    )
    
    if (nrow(concept_names) > 0) {
      features_with_names <- features_data %>%
        dplyr::left_join(concept_names, by = "concept_id") %>%
        dplyr::mutate(
          concept_name = ifelse(is.na(.data$concept_name), 
                               paste("Unknown concept", .data$concept_id), 
                               .data$concept_name)
        ) %>%
        dplyr::select(.data$person_id, .data$concept_id, .data$concept_name, 
                     dplyr::everything(), -.data$vocabulary_id, -.data$concept_class_id)
      
      return(features_with_names)
    }
    
  }, error = function(e) {
    cli::cli_alert_info("Could not retrieve concept names: {e$message}")
  })
  
  # Add placeholder concept names if query failed
  features_data$concept_name <- paste("Concept", features_data$concept_id)
  return(features_data %>% dplyr::select(.data$person_id, .data$concept_id, .data$concept_name, dplyr::everything()))
}


#' Create Feature Matrix for PheProb Calculation
#'
#' Converts extracted EHR features into a wide-format matrix suitable for
#' PheProb calculations.
#'
#' @param features_data Tibble with extracted features from extract_ehr_features
#' @param feature_type Type of feature to use as values. Options:
#'   \itemize{
#'     \item "binary": 1 if concept present, 0 if absent
#'     \item "count": occurrence_count values
#'     \item "log_count": log(occurrence_count + 1)
#'     \item "recency": days since last occurrence (inverted)
#'   }
#' @param fill_missing Value to use for missing person-concept combinations (default: 0)
#'
#' @return A matrix with:
#'   \item{rows}{person_ids}
#'   \item{columns}{concept_ids with concept names}
#'   \item{values}{calculated feature values}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' features <- extract_ehr_features(c(201826, 4329847))
#' feature_matrix <- create_feature_matrix(features, feature_type = "binary")
#' }
create_feature_matrix <- function(features_data, feature_type = "binary", fill_missing = 0) {
  
  if (nrow(features_data) == 0) {
    cli::cli_alert_info("No features data provided, returning empty matrix")
    return(matrix(nrow = 0, ncol = 0))
  }
  
  # Calculate feature values based on type
  features_with_values <- features_data %>%
    dplyr::mutate(
      feature_value = switch(feature_type,
        "binary" = 1,
        "count" = .data$occurrence_count,
        "log_count" = log(.data$occurrence_count + 1),
        "recency" = pmax(0, 365 - as.numeric(Sys.Date() - .data$last_occurrence_date)),
        stop("Invalid feature_type. Must be one of: binary, count, log_count, recency")
      )
    )
  
  # Create wide format matrix
  feature_wide <- features_with_values %>%
    dplyr::select(.data$person_id, .data$concept_id, .data$concept_name, .data$feature_value) %>%
    # In case of duplicates, take the maximum value
    dplyr::group_by(.data$person_id, .data$concept_id, .data$concept_name) %>%
    dplyr::summarise(feature_value = max(.data$feature_value, na.rm = TRUE), .groups = "drop") %>%
    # Create concept labels with both ID and name
    dplyr::mutate(concept_label = paste0(.data$concept_id, "_", gsub("[^A-Za-z0-9_]", "_", .data$concept_name))) %>%
    dplyr::select(.data$person_id, .data$concept_label, .data$feature_value) %>%
    tidyr::pivot_wider(
      names_from = .data$concept_label, 
      values_from = .data$feature_value, 
      values_fill = fill_missing
    )
  
  # Convert to matrix
  person_ids <- feature_wide$person_id
  feature_matrix <- as.matrix(feature_wide[, -1])
  rownames(feature_matrix) <- as.character(person_ids)
  
  cli::cli_alert_success("Created feature matrix: {nrow(feature_matrix)} persons × {ncol(feature_matrix)} concepts")
  
  return(feature_matrix)
}


#' Export Feature Data to File
#'
#' Exports extracted features or feature matrix to various file formats.
#'
#' @param data Feature data (tibble) or feature matrix to export
#' @param output_file Path to output file. Extension determines format (.csv, .rds)
#' @param include_metadata Whether to include metadata about the extraction
#'
#' @return Logical indicating success
#'
#' @export
#'
#' @examples
#' \dontrun{
#' features <- extract_ehr_features(c(201826, 4329847))
#' export_features(features, "my_features.csv")
#' }
export_features <- function(data, output_file, include_metadata = TRUE) {
  
  if (missing(output_file) || is.null(output_file)) {
    cli::cli_abort("output_file must be specified")
  }
  
  file_ext <- tools::file_ext(output_file)
  
  tryCatch({
    
    if (file_ext == "csv") {
      readr::write_csv(data, output_file)
      
    } else if (file_ext == "rds") {
      if (include_metadata) {
        output_data <- list(
          data = data,
          metadata = list(
            extraction_date = Sys.time(),
            package_version = utils::packageVersion("pheprobAoU"),
            n_persons = if (is.matrix(data)) nrow(data) else length(unique(data$person_id)),
            n_concepts = if (is.matrix(data)) ncol(data) else length(unique(data$concept_id))
          )
        )
        saveRDS(output_data, output_file)
      } else {
        saveRDS(data, output_file)
      }
      
    } else {
      cli::cli_abort("Unsupported file format. Use .csv or .rds")
    }
    
    cli::cli_alert_success("Data exported to: {output_file}")
    return(TRUE)
    
  }, error = function(e) {
    cli::cli_abort("Error exporting data: {e$message}")
  })
}
