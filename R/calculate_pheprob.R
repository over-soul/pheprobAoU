#' Calculate PheProb (Phenotype Probabilities) using All of Us EHR Data
#'
#' This is the main function for calculating PheProb scores using electronic 
#' health record data from the All of Us Research Program. PheProb provides
#' probabilistic phenotype predictions based on the presence, frequency, and
#' patterns of clinical concepts in a patient's medical history.
#'
#' @param concept_ids A numeric vector of OMOP concept IDs to use for PheProb
#'   calculation. These concepts define the phenotype of interest.
#' @param person_ids A numeric vector of person IDs to include in the analysis.
#'   If NULL (default), includes all available persons in the database.
#' @param method Character string specifying the PheProb calculation method:
#'   \itemize{
#'     \item "simple": Binary presence/absence scoring (0 or 1)
#'     \item "weighted": Frequency-weighted scoring using occurrence counts
#'     \item "temporal": Time-aware scoring considering recency of events
#'     \item "composite": Combines multiple scoring approaches (default)
#'   }
#' @param domains Character vector specifying which OMOP domains to search.
#'   Default: c("condition", "procedure", "drug", "measurement", "observation")
#' @param date_range Optional list with 'start' and 'end' dates (Date objects)
#'   to limit the temporal scope of data extraction.
#' @param weights Optional named numeric vector providing custom weights for
#'   concept IDs. Names should match concept_ids. If NULL, equal weights are used.
#' @param normalization Character string specifying score normalization:
#'   \itemize{
#'     \item "none": No normalization (raw scores)
#'     \item "minmax": Min-max scaling to [0,1] range (default)
#'     \item "zscore": Z-score standardization
#'     \item "logistic": Logistic transformation
#'   }
#' @param output_format Character string specifying output format:
#'   \itemize{
#'     \item "matrix": Returns a matrix with persons as rows, concepts as columns
#'     \item "long": Returns a long-format tibble with person_id, concept_id, score
#'     \item "wide": Returns a wide-format tibble (default)
#'   }
#' @param output_file Optional file path to save results. Format determined by
#'   file extension (.csv, .rds).
#' @param batch_size Integer specifying batch size for processing large datasets
#'   (default: 10000). Larger batches are faster but use more memory.
#' @param progress Logical indicating whether to show progress bars (default: TRUE)
#' @param ... Additional arguments passed to underlying functions
#'
#' @return Depending on output_format:
#'   \item{matrix}{Matrix with person IDs as rownames, concept IDs as colnames}
#'   \item{long}{Tibble with columns: person_id, concept_id, concept_name, pheprob_score}
#'   \item{wide}{Tibble with person_id column and one column per concept}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic diabetes phenotyping using condition concepts
#' diabetes_concepts <- c(201826, 4329847, 9201)  # Type 2 diabetes concepts
#' 
#' # Simple binary scoring
#' diabetes_scores_simple <- calculate_pheprob(
#'   concept_ids = diabetes_concepts,
#'   method = "simple"
#' )
#' 
#' # Weighted scoring with custom weights
#' diabetes_scores_weighted <- calculate_pheprob(
#'   concept_ids = diabetes_concepts,
#'   method = "weighted",
#'   weights = c("201826" = 2.0, "4329847" = 1.5, "9201" = 1.0)
#' )
#' 
#' # Temporal scoring with date range
#' recent_diabetes <- calculate_pheprob(
#'   concept_ids = diabetes_concepts,
#'   method = "temporal",
#'   date_range = list(start = as.Date("2020-01-01"), end = Sys.Date())
#' )
#' 
#' # Export results to CSV
#' calculate_pheprob(
#'   concept_ids = diabetes_concepts,
#'   method = "composite",
#'   output_file = "diabetes_pheprob_scores.csv"
#' )
#' }
calculate_pheprob <- function(concept_ids,
                             person_ids = NULL,
                             method = "composite",
                             domains = c("condition", "procedure", "drug", "measurement", "observation"),
                             date_range = NULL,
                             weights = NULL,
                             normalization = "minmax",
                             output_format = "wide",
                             output_file = NULL,
                             batch_size = 10000,
                             progress = TRUE,
                             ...) {
  
  # Start timing
  start_time <- Sys.time()
  
  if (progress) {
    cli::cli_alert_info("Starting PheProb calculation for {length(concept_ids)} concept(s)")
  }
  
  # Validate inputs
  validation_results <- validate_parameters(
    concept_ids = concept_ids,
    person_ids = person_ids,
    output_file = output_file
  )
  
  validated_concept_ids <- validation_results$validated_concept_ids
  validated_person_ids <- validation_results$validated_person_ids
  
  if (progress) {
    cli::cli_alert_success("Validation complete: {length(validated_concept_ids)} valid concept(s), {length(validated_person_ids)} person(s)")
  }
  
  # Validate method
  valid_methods <- c("simple", "weighted", "temporal", "composite")
  if (!method %in% valid_methods) {
    cli::cli_abort("Invalid method. Must be one of: {paste(valid_methods, collapse = ', ')}")
  }
  
  # Validate normalization
  valid_normalizations <- c("none", "minmax", "zscore", "logistic")
  if (!normalization %in% valid_normalizations) {
    cli::cli_abort("Invalid normalization. Must be one of: {paste(valid_normalizations, collapse = ', ')}")
  }
  
  # Validate output format
  valid_formats <- c("matrix", "long", "wide")
  if (!output_format %in% valid_formats) {
    cli::cli_abort("Invalid output_format. Must be one of: {paste(valid_formats, collapse = ', ')}")
  }
  
  # Process in batches for large datasets
  if (!is.null(validated_person_ids) && length(validated_person_ids) > batch_size) {
    if (progress) {
      cli::cli_alert_info("Processing {length(validated_person_ids)} persons in batches of {batch_size}")
    }
    
    person_batches <- split(validated_person_ids, ceiling(seq_along(validated_person_ids) / batch_size))
    batch_results <- list()
    
    if (progress) {
      cli::cli_progress_bar("Processing batches", total = length(person_batches))
    }
    
    for (i in seq_along(person_batches)) {
      if (progress) {
        cli::cli_progress_update()
      }
      
      batch_result <- calculate_pheprob_batch(
        concept_ids = validated_concept_ids,
        person_ids = person_batches[[i]],
        method = method,
        domains = domains,
        date_range = date_range,
        weights = weights,
        progress = FALSE
      )
      
      batch_results[[i]] <- batch_result
    }
    
    if (progress) {
      cli::cli_progress_done()
    }
    
    # Combine batch results
    combined_scores <- combine_batch_results(batch_results)
    
  } else {
    # Process all at once
    combined_scores <- calculate_pheprob_batch(
      concept_ids = validated_concept_ids,
      person_ids = validated_person_ids,
      method = method,
      domains = domains,
      date_range = date_range,
      weights = weights,
      progress = progress
    )
  }
  
  # Apply normalization
  if (normalization != "none") {
    combined_scores <- normalize_scores(combined_scores, method = normalization)
    if (progress) {
      cli::cli_alert_info("Applied {normalization} normalization")
    }
  }
  
  # Format output
  formatted_output <- format_pheprob_output(
    scores = combined_scores,
    format = output_format,
    concept_ids = validated_concept_ids
  )
  
  # Save to file if requested
  if (!is.null(output_file)) {
    export_features(formatted_output, output_file)
  }
  
  # Performance summary
  end_time <- Sys.time()
  processing_time <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 2)
  
  if (progress) {
    n_persons <- if (is.matrix(formatted_output)) nrow(formatted_output) else length(unique(formatted_output$person_id))
    n_concepts <- length(validated_concept_ids)
    
    cli::cli_alert_success("PheProb calculation complete!")
    cli::cli_alert_info("Processed {n_persons} persons × {n_concepts} concepts in {processing_time}s")
  }
  
  return(formatted_output)
}


#' Calculate PheProb Scores for a Batch of Data
#'
#' Internal function to calculate PheProb scores for a specific batch of persons.
#'
#' @param concept_ids Validated concept IDs
#' @param person_ids Person IDs for this batch
#' @param method Calculation method
#' @param domains OMOP domains to search
#' @param date_range Date range filter
#' @param weights Custom weights
#' @param progress Show progress
#'
#' @return Tibble with calculated scores
#'
#' @keywords internal
calculate_pheprob_batch <- function(concept_ids, person_ids, method, domains, 
                                   date_range, weights, progress) {
  
  # Extract EHR features
  features <- extract_ehr_features(
    concept_ids = concept_ids,
    person_ids = person_ids,
    domains = domains,
    date_range = date_range
  )
  
  if (nrow(features) == 0) {
    cli::cli_alert_info("No features found for this batch")
    return(create_empty_scores_tibble(concept_ids))
  }
  
  # Calculate scores based on method
  scores <- switch(method,
    "simple" = calculate_simple_scores(features, concept_ids, weights),
    "weighted" = calculate_weighted_scores(features, concept_ids, weights),
    "temporal" = calculate_temporal_scores(features, concept_ids, weights),
    "composite" = calculate_composite_scores(features, concept_ids, weights)
  )
  
  return(scores)
}


#' Calculate Simple Binary PheProb Scores
#'
#' Calculates simple binary scores based on concept presence/absence.
#'
#' @param features Feature data from extract_ehr_features
#' @param concept_ids Vector of concept IDs
#' @param weights Optional concept weights
#'
#' @return Tibble with scores
#'
#' @keywords internal
calculate_simple_scores <- function(features, concept_ids, weights = NULL) {
  
  # Create binary feature matrix
  feature_matrix <- create_feature_matrix(features, feature_type = "binary")
  
  if (nrow(feature_matrix) == 0) {
    return(create_empty_scores_tibble(concept_ids))
  }
  
  # Apply weights if provided
  if (!is.null(weights)) {
    feature_matrix <- apply_concept_weights(feature_matrix, weights)
  }
  
  # Calculate simple sum scores
  person_scores <- rowSums(feature_matrix, na.rm = TRUE)
  
  # Create results tibble
  scores_tibble <- tibble::tibble(
    person_id = as.numeric(names(person_scores)),
    pheprob_score = as.numeric(person_scores)
  )
  
  # Add individual concept scores
  for (i in seq_len(ncol(feature_matrix))) {
    concept_col <- colnames(feature_matrix)[i]
    scores_tibble[[concept_col]] <- feature_matrix[, i]
  }
  
  return(scores_tibble)
}


#' Calculate Weighted PheProb Scores
#'
#' Calculates frequency-weighted scores based on occurrence counts.
#'
#' @param features Feature data
#' @param concept_ids Vector of concept IDs
#' @param weights Optional concept weights
#'
#' @return Tibble with scores
#'
#' @keywords internal
calculate_weighted_scores <- function(features, concept_ids, weights = NULL) {
  
  # Create count-based feature matrix
  feature_matrix <- create_feature_matrix(features, feature_type = "log_count")
  
  if (nrow(feature_matrix) == 0) {
    return(create_empty_scores_tibble(concept_ids))
  }
  
  # Apply weights if provided
  if (!is.null(weights)) {
    feature_matrix <- apply_concept_weights(feature_matrix, weights)
  }
  
  # Calculate weighted sum scores
  person_scores <- rowSums(feature_matrix, na.rm = TRUE)
  
  # Create results tibble
  scores_tibble <- tibble::tibble(
    person_id = as.numeric(names(person_scores)),
    pheprob_score = as.numeric(person_scores)
  )
  
  # Add individual concept scores
  for (i in seq_len(ncol(feature_matrix))) {
    concept_col <- colnames(feature_matrix)[i]
    scores_tibble[[concept_col]] <- feature_matrix[, i]
  }
  
  return(scores_tibble)
}


#' Calculate Temporal PheProb Scores
#'
#' Calculates time-aware scores considering recency of events.
#'
#' @param features Feature data
#' @param concept_ids Vector of concept IDs
#' @param weights Optional concept weights
#'
#' @return Tibble with scores
#'
#' @keywords internal
calculate_temporal_scores <- function(features, concept_ids, weights = NULL) {
  
  # Create recency-based feature matrix
  feature_matrix <- create_feature_matrix(features, feature_type = "recency")
  
  if (nrow(feature_matrix) == 0) {
    return(create_empty_scores_tibble(concept_ids))
  }
  
  # Apply temporal decay (more recent = higher score)
  feature_matrix <- apply(feature_matrix, 2, function(x) {
    ifelse(x > 0, exp(-x / 365), 0)  # Exponential decay over years
  })
  
  # Apply weights if provided
  if (!is.null(weights)) {
    feature_matrix <- apply_concept_weights(feature_matrix, weights)
  }
  
  # Calculate temporal sum scores
  person_scores <- rowSums(feature_matrix, na.rm = TRUE)
  
  # Create results tibble
  scores_tibble <- tibble::tibble(
    person_id = as.numeric(rownames(feature_matrix)),
    pheprob_score = as.numeric(person_scores)
  )
  
  # Add individual concept scores
  for (i in seq_len(ncol(feature_matrix))) {
    concept_col <- colnames(feature_matrix)[i]
    scores_tibble[[concept_col]] <- feature_matrix[, i]
  }
  
  return(scores_tibble)
}


#' Calculate Composite PheProb Scores
#'
#' Calculates composite scores combining multiple approaches.
#'
#' @param features Feature data
#' @param concept_ids Vector of concept IDs
#' @param weights Optional concept weights
#'
#' @return Tibble with scores
#'
#' @keywords internal
calculate_composite_scores <- function(features, concept_ids, weights = NULL) {
  
  # Calculate individual score components
  simple_scores <- calculate_simple_scores(features, concept_ids, weights)
  weighted_scores <- calculate_weighted_scores(features, concept_ids, weights)
  temporal_scores <- calculate_temporal_scores(features, concept_ids, weights)
  
  # Combine scores with equal weighting
  composite_scores <- simple_scores %>%
    dplyr::left_join(
      weighted_scores %>% dplyr::select(.data$person_id, weighted_score = .data$pheprob_score),
      by = "person_id"
    ) %>%
    dplyr::left_join(
      temporal_scores %>% dplyr::select(.data$person_id, temporal_score = .data$pheprob_score),
      by = "person_id"
    ) %>%
    dplyr::mutate(
      pheprob_score = (.data$pheprob_score + .data$weighted_score + .data$temporal_score) / 3
    ) %>%
    dplyr::select(-.data$weighted_score, -.data$temporal_score)
  
  return(composite_scores)
}


#' Apply Concept Weights to Feature Matrix
#'
#' Applies custom weights to specific concepts in the feature matrix.
#'
#' @param feature_matrix Matrix with features
#' @param weights Named vector of weights
#'
#' @return Weighted feature matrix
#'
#' @keywords internal
apply_concept_weights <- function(feature_matrix, weights) {
  
  if (is.null(weights) || length(weights) == 0) {
    return(feature_matrix)
  }
  
  # Match weights to column names (concept IDs)
  for (weight_name in names(weights)) {
    # Find columns that match this concept ID
    matching_cols <- grepl(paste0("^", weight_name, "_"), colnames(feature_matrix))
    
    if (any(matching_cols)) {
      feature_matrix[, matching_cols] <- feature_matrix[, matching_cols] * weights[weight_name]
    }
  }
  
  return(feature_matrix)
}


#' Normalize PheProb Scores
#'
#' Applies normalization to PheProb scores.
#'
#' @param scores_tibble Tibble with scores
#' @param method Normalization method
#'
#' @return Normalized scores tibble
#'
#' @keywords internal
normalize_scores <- function(scores_tibble, method) {
  
  score_col <- "pheprob_score"
  scores <- scores_tibble[[score_col]]
  
  normalized_scores <- switch(method,
    "minmax" = {
      min_score <- min(scores, na.rm = TRUE)
      max_score <- max(scores, na.rm = TRUE)
      if (max_score > min_score) {
        (scores - min_score) / (max_score - min_score)
      } else {
        rep(0, length(scores))
      }
    },
    "zscore" = {
      mean_score <- mean(scores, na.rm = TRUE)
      sd_score <- stats::sd(scores, na.rm = TRUE)
      if (sd_score > 0) {
        (scores - mean_score) / sd_score
      } else {
        rep(0, length(scores))
      }
    },
    "logistic" = {
      1 / (1 + exp(-scores))
    },
    scores  # Default: no normalization
  )
  
  scores_tibble[[score_col]] <- normalized_scores
  return(scores_tibble)
}


#' Format PheProb Output
#'
#' Formats PheProb scores into the requested output format.
#'
#' @param scores Scores tibble
#' @param format Output format
#' @param concept_ids Original concept IDs
#'
#' @return Formatted output
#'
#' @keywords internal
format_pheprob_output <- function(scores, format, concept_ids) {
  
  if (nrow(scores) == 0) {
    return(create_empty_output(format, concept_ids))
  }
  
  switch(format,
    "matrix" = {
      # Convert to matrix format
      concept_cols <- setdiff(colnames(scores), c("person_id", "pheprob_score"))
      if (length(concept_cols) > 0) {
        score_matrix <- as.matrix(scores[, concept_cols])
        rownames(score_matrix) <- as.character(scores$person_id)
        return(score_matrix)
      } else {
        # Fallback: create matrix with just PheProb scores
        score_matrix <- matrix(scores$pheprob_score, ncol = 1)
        rownames(score_matrix) <- as.character(scores$person_id)
        colnames(score_matrix) <- "pheprob_score"
        return(score_matrix)
      }
    },
    "long" = {
      # Convert to long format
      concept_cols <- setdiff(colnames(scores), c("person_id", "pheprob_score"))
      if (length(concept_cols) > 0) {
        scores %>%
          tidyr::pivot_longer(
            cols = dplyr::all_of(concept_cols),
            names_to = "concept_label",
            values_to = "concept_score"
          ) %>%
          dplyr::mutate(
            concept_id = as.numeric(stringr::str_extract(.data$concept_label, "^[0-9]+"))
          ) %>%
          dplyr::select(.data$person_id, .data$concept_id, .data$concept_label, 
                       .data$concept_score, .data$pheprob_score)
      } else {
        scores %>%
          dplyr::mutate(concept_id = NA_real_, concept_label = "overall", concept_score = .data$pheprob_score) %>%
          dplyr::select(.data$person_id, .data$concept_id, .data$concept_label, 
                       .data$concept_score, .data$pheprob_score)
      }
    },
    "wide" = {
      # Already in wide format
      return(scores)
    }
  )
}


#' Create Empty Scores Tibble
#'
#' Creates an empty scores tibble with the correct structure.
#'
#' @param concept_ids Vector of concept IDs
#'
#' @return Empty tibble
#'
#' @keywords internal
create_empty_scores_tibble <- function(concept_ids) {
  tibble::tibble(
    person_id = numeric(0),
    pheprob_score = numeric(0)
  )
}


#' Create Empty Output
#'
#' Creates empty output in the requested format.
#'
#' @param format Output format
#' @param concept_ids Vector of concept IDs
#'
#' @return Empty output object
#'
#' @keywords internal
create_empty_output <- function(format, concept_ids) {
  switch(format,
    "matrix" = matrix(nrow = 0, ncol = length(concept_ids)),
    "long" = tibble::tibble(
      person_id = numeric(0),
      concept_id = numeric(0),
      concept_label = character(0),
      concept_score = numeric(0),
      pheprob_score = numeric(0)
    ),
    "wide" = tibble::tibble(person_id = numeric(0), pheprob_score = numeric(0))
  )
}


#' Combine Batch Results
#'
#' Combines results from multiple processing batches.
#'
#' @param batch_results List of batch result tibbles
#'
#' @return Combined tibble
#'
#' @keywords internal
combine_batch_results <- function(batch_results) {
  
  valid_results <- batch_results[sapply(batch_results, function(x) nrow(x) > 0)]
  
  if (length(valid_results) == 0) {
    return(tibble::tibble(person_id = numeric(0), pheprob_score = numeric(0)))
  }
  
  dplyr::bind_rows(valid_results)
}


#' Calculate PheProb for Multiple Phenotypes
#'
#' Calculates separate PheProb scores for multiple related phenotypes, avoiding
#' the problem of mixing unrelated concept IDs in a single analysis. Each
#' phenotype gets its own probability score, making results more clinically
#' interpretable.
#'
#' @param phenotype_concepts A named list where each element contains concept IDs
#'   for a specific phenotype. Names will be used as column names in the output.
#'   Example: list(diabetes = c(201826, 4329847), cvd = c(314866, 313217))
#' @param person_ids A numeric vector of person IDs to include in the analysis.
#'   If NULL (default), includes all available persons in the database.
#' @param method Character string specifying the PheProb calculation method.
#'   Same options as calculate_pheprob(): "simple", "weighted", "temporal", "composite"
#' @param domains Character vector specifying which OMOP domains to search.
#'   Default: c("condition", "procedure", "drug", "measurement", "observation")
#' @param date_range Optional list with 'start' and 'end' dates (Date objects)
#'   to limit the temporal scope of data extraction.
#' @param phenotype_weights Optional named list providing custom weights for
#'   concept IDs within each phenotype. Names should match phenotype names.
#'   Example: list(diabetes = c("201826" = 2.0, "4329847" = 1.5))
#' @param normalization Character string specifying score normalization.
#'   Options: "none", "minmax", "zscore", "logistic"
#' @param output_format Character string specifying output format:
#'   \itemize{
#'     \item "wide": Returns wide-format tibble with separate columns per phenotype (default)
#'     \item "long": Returns long-format tibble with phenotype_name column
#'   }
#' @param output_file Optional file path to save results.
#' @param batch_size Integer specifying batch size for processing large datasets
#' @param progress Logical indicating whether to show progress bars (default: TRUE)
#' @param ... Additional arguments passed to underlying functions
#'
#' @return Depending on output_format:
#'   \item{wide}{Tibble with person_id and one probability column per phenotype}
#'   \item{long}{Tibble with person_id, phenotype_name, pheprob_score columns}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Define multiple phenotypes
#' phenotypes <- list(
#'   diabetes = c(201826, 4329847, 9201),
#'   cvd = c(314866, 313217, 316866),
#'   depression = c(4152280, 4226263)
#' )
#' 
#' # Calculate separate probabilities for each phenotype
#' multi_scores <- calculate_multiple_pheprobs(
#'   phenotype_concepts = phenotypes,
#'   method = "composite"
#' )
#' 
#' # View results - each phenotype gets its own probability column
#' head(multi_scores)
#' #   person_id diabetes_prob cvd_prob depression_prob
#' #      123456          0.89     0.45            0.12
#' #      234567          0.23     0.91            0.67
#' 
#' # With custom weights per phenotype
#' phenotype_weights <- list(
#'   diabetes = c("201826" = 2.0, "4329847" = 1.5, "9201" = 1.0),
#'   cvd = c("314866" = 3.0, "313217" = 2.0, "316866" = 1.5)
#' )
#' 
#' weighted_scores <- calculate_multiple_pheprobs(
#'   phenotype_concepts = phenotypes,
#'   method = "weighted",
#'   phenotype_weights = phenotype_weights
#' )
#' }
calculate_multiple_pheprobs <- function(phenotype_concepts,
                                       person_ids = NULL,
                                       method = "composite",
                                       domains = c("condition", "procedure", "drug", "measurement", "observation"),
                                       date_range = NULL,
                                       phenotype_weights = NULL,
                                       normalization = "minmax",
                                       output_format = "wide",
                                       output_file = NULL,
                                       batch_size = 10000,
                                       progress = TRUE,
                                       ...) {
  
  # Start timing
  start_time <- Sys.time()
  
  # Validate inputs
  if (missing(phenotype_concepts) || is.null(phenotype_concepts) || length(phenotype_concepts) == 0) {
    cli::cli_abort("phenotype_concepts must be provided as a named list")
  }
  
  if (!is.list(phenotype_concepts)) {
    cli::cli_abort("phenotype_concepts must be a list")
  }
  
  if (is.null(names(phenotype_concepts)) || any(names(phenotype_concepts) == "")) {
    cli::cli_abort("All elements in phenotype_concepts must be named")
  }
  
  # Check for valid phenotype names (no spaces, special characters)
  phenotype_names <- names(phenotype_concepts)
  invalid_names <- phenotype_names[!grepl("^[A-Za-z][A-Za-z0-9_]*$", phenotype_names)]
  if (length(invalid_names) > 0) {
    cli::cli_abort("Invalid phenotype names: {paste(invalid_names, collapse = ', ')}. Names must start with a letter and contain only letters, numbers, and underscores.")
  }
  
  # Validate output format
  valid_formats <- c("wide", "long")
  if (!output_format %in% valid_formats) {
    cli::cli_abort("Invalid output_format for multiple phenotypes. Must be one of: {paste(valid_formats, collapse = ', ')}")
  }
  
  if (progress) {
    cli::cli_alert_info("Starting multi-phenotype PheProb calculation for {length(phenotype_concepts)} phenotype(s)")
    cli::cli_alert_info("Phenotypes: {paste(phenotype_names, collapse = ', ')}")
  }
  
  # Initialize results storage
  phenotype_results <- list()
  
  # Set up progress bar
  if (progress) {
    cli::cli_progress_bar("Processing phenotypes", total = length(phenotype_concepts))
  }
  
  # Calculate PheProb for each phenotype
  for (phenotype_name in phenotype_names) {
    
    if (progress) {
      cli::cli_progress_update()
      cli::cli_alert_info("Processing {phenotype_name} phenotype...")
    }
    
    concept_ids <- phenotype_concepts[[phenotype_name]]
    
    # Get weights for this phenotype if provided
    weights <- NULL
    if (!is.null(phenotype_weights) && phenotype_name %in% names(phenotype_weights)) {
      weights <- phenotype_weights[[phenotype_name]]
    }
    
    # Calculate PheProb for this phenotype
    tryCatch({
      phenotype_scores <- calculate_pheprob(
        concept_ids = concept_ids,
        person_ids = person_ids,
        method = method,
        domains = domains,
        date_range = date_range,
        weights = weights,
        normalization = normalization,
        output_format = "wide",  # Always use wide internally
        batch_size = batch_size,
        progress = FALSE  # Suppress individual progress
      )
      
      # Extract just the person_id and overall score
      phenotype_result <- phenotype_scores %>%
        dplyr::select(.data$person_id, .data$pheprob_score) %>%
        dplyr::rename(!!paste0(phenotype_name, "_prob") := .data$pheprob_score)
      
      phenotype_results[[phenotype_name]] <- phenotype_result
      
      if (progress) {
        n_persons <- nrow(phenotype_result)
        mean_score <- round(mean(phenotype_result[[paste0(phenotype_name, "_prob")]], na.rm = TRUE), 3)
        cli::cli_alert_success("✓ {phenotype_name}: {n_persons} persons, mean score = {mean_score}")
      }
      
    }, error = function(e) {
      if (progress) {
        cli::cli_alert_info("✗ Error processing {phenotype_name}: {e$message}")
      }
      # Store empty result to maintain structure
      phenotype_results[[phenotype_name]] <- tibble::tibble(
        person_id = numeric(0),
        !!paste0(phenotype_name, "_prob") := numeric(0)
      )
    })
  }
  
  if (progress) {
    cli::cli_progress_done()
  }
  
  # Combine results
  if (length(phenotype_results) == 0) {
    cli::cli_abort("No valid phenotype results generated")
  }
  
  # Start with first phenotype result
  combined_results <- phenotype_results[[1]]
  
  # Join with remaining phenotypes
  if (length(phenotype_results) > 1) {
    for (i in 2:length(phenotype_results)) {
      combined_results <- dplyr::full_join(
        combined_results, 
        phenotype_results[[i]], 
        by = "person_id"
      )
    }
  }
  
  # Replace NA values with 0 (no evidence for phenotype)
  prob_columns <- paste0(phenotype_names, "_prob")
  combined_results <- combined_results %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(prob_columns), ~ ifelse(is.na(.x), 0, .x))
    )
  
  # Format output
  if (output_format == "long") {
    combined_results <- combined_results %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(prob_columns),
        names_to = "phenotype_name",
        values_to = "pheprob_score"
      ) %>%
      dplyr::mutate(
        phenotype_name = stringr::str_remove(.data$phenotype_name, "_prob$")
      )
  }
  
  # Add summary statistics
  if (progress) {
    n_total_persons <- length(unique(combined_results$person_id))
    
    if (output_format == "wide") {
      # Calculate summary for wide format
      summary_stats <- combined_results %>%
        dplyr::select(-person_id) %>%
        dplyr::summarise_all(list(
          mean = ~ round(mean(.x, na.rm = TRUE), 3),
          high_prob = ~ sum(.x > 0.8, na.rm = TRUE)
        ), .groups = "drop")
      
      cli::cli_alert_success("Multi-phenotype analysis complete!")
      cli::cli_alert_info("Total persons: {n_total_persons}")
      
      for (phenotype in phenotype_names) {
        mean_col <- paste0(phenotype, "_prob_mean")
        high_col <- paste0(phenotype, "_prob_high_prob")
        if (mean_col %in% names(summary_stats)) {
          cli::cli_alert_info("{phenotype}: mean = {summary_stats[[mean_col]]}, high probability cases = {summary_stats[[high_col]]}")
        }
      }
      
    } else {
      # Long format summary
      summary_by_phenotype <- combined_results %>%
        dplyr::group_by(.data$phenotype_name) %>%
        dplyr::summarise(
          mean_score = round(mean(.data$pheprob_score, na.rm = TRUE), 3),
          high_prob_cases = sum(.data$pheprob_score > 0.8, na.rm = TRUE),
          .groups = "drop"
        )
      
      cli::cli_alert_success("Multi-phenotype analysis complete!")
      cli::cli_alert_info("Total persons: {n_total_persons}")
      print(summary_by_phenotype)
    }
  }
  
  # Save to file if requested
  if (!is.null(output_file)) {
    export_features(combined_results, output_file)
  }
  
  # Performance summary
  end_time <- Sys.time()
  processing_time <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 2)
  
  if (progress) {
    cli::cli_alert_info("Total processing time: {processing_time}s")
  }
  
  return(combined_results)
}


#' Validate Phenotype Concept Coherence
#'
#' Validates that concept IDs within each phenotype are clinically coherent
#' and warns about potential issues with mixed or unrelated concepts.
#'
#' @param phenotype_concepts Named list of concept ID vectors
#' @param check_domains Logical, whether to check domain coherence
#' @param max_domains_per_phenotype Maximum number of domains allowed per phenotype
#'   before issuing a warning (default: 3)
#'
#' @return List with validation results for each phenotype
#'
#' @export
#'
#' @examples
#' \dontrun{
#' phenotypes <- list(
#'   diabetes = c(201826, 4329847, 9201),
#'   mixed_example = c(201826, 313217, 432870)  # Mixed: diabetes, AFib, depression
#' )
#' 
#' validation <- validate_phenotype_coherence(phenotypes)
#' print(validation$summary)
#' }
validate_phenotype_coherence <- function(phenotype_concepts, 
                                        check_domains = TRUE,
                                        max_domains_per_phenotype = 3) {
  
  if (!is.list(phenotype_concepts) || is.null(names(phenotype_concepts))) {
    cli::cli_abort("phenotype_concepts must be a named list")
  }
  
  validation_results <- list()
  overall_warnings <- c()
  
  for (phenotype_name in names(phenotype_concepts)) {
    concept_ids <- phenotype_concepts[[phenotype_name]]
    
    # Validate individual concept IDs
    concept_validation <- validate_concept_ids(concept_ids, check_existence = check_domains)
    
    phenotype_result <- list(
      phenotype_name = phenotype_name,
      concept_validation = concept_validation,
      warnings = c()
    )
    
    if (check_domains && concept_validation$summary$valid_concept_ids > 0) {
      
      tryCatch({
        # Query concept information for domain analysis
        valid_concepts <- concept_validation$validated_concept_ids
        
        concept_info <- allofus::aou_sql(glue::glue("
          SELECT concept_id, domain_id, vocabulary_id, concept_class_id, concept_name
          FROM concept 
          WHERE concept_id IN ({paste(valid_concepts, collapse = ', ')})
        "))
        
        if (nrow(concept_info) > 0) {
          unique_domains <- unique(concept_info$domain_id)
          unique_vocabularies <- unique(concept_info$vocabulary_id)
          
          # Check domain diversity
          if (length(unique_domains) > max_domains_per_phenotype) {
            warning_msg <- glue::glue(
              "{phenotype_name}: Concepts span {length(unique_domains)} domains ({paste(unique_domains, collapse = ', ')}). Consider reviewing concept selection."
            )
            phenotype_result$warnings <- c(phenotype_result$warnings, warning_msg)
            overall_warnings <- c(overall_warnings, warning_msg)
          }
          
          # Check for very diverse vocabularies (potential sign of mixing unrelated concepts)
          if (length(unique_vocabularies) > 4) {
            warning_msg <- glue::glue(
              "{phenotype_name}: Concepts from {length(unique_vocabularies)} different vocabularies. This may indicate mixed concept types."
            )
            phenotype_result$warnings <- c(phenotype_result$warnings, warning_msg)
            overall_warnings <- c(overall_warnings, warning_msg)
          }
          
          phenotype_result$domain_info <- list(
            domains = unique_domains,
            vocabularies = unique_vocabularies,
            concept_details = concept_info
          )
        }
        
      }, error = function(e) {
        cli::cli_alert_info("Could not perform domain analysis for {phenotype_name}: {e$message}")
      })
    }
    
    validation_results[[phenotype_name]] <- phenotype_result
  }
  
  # Print warnings
  if (length(overall_warnings) > 0) {
    cli::cli_alert_info("Phenotype coherence validation completed with warnings:")
    for (warning in overall_warnings) {
      cli::cli_alert_warning(warning)
    }
  } else {
    cli::cli_alert_success("All phenotypes passed coherence validation")
  }
  
  # Create summary
  summary_stats <- data.frame(
    phenotype_name = names(phenotype_concepts),
    n_concepts = sapply(phenotype_concepts, length),
    n_valid_concepts = sapply(validation_results, function(x) x$concept_validation$summary$valid_concept_ids),
    n_warnings = sapply(validation_results, function(x) length(x$warnings)),
    stringsAsFactors = FALSE
  )
  
  return(list(
    individual_results = validation_results,
    summary = summary_stats,
    overall_warnings = overall_warnings,
    validation_passed = length(overall_warnings) == 0
  ))
}
