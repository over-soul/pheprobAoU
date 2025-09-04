#' Multiple Phenotypes Implementation (Sinnott et al., 2018)
#'
#' This file implements multiple phenotype analysis using the PheProb
#' binomial mixture model methodology (Sinnott et al., 2018). Each phenotype gets its own binomial
#' mixture model fitted independently.
#'
#' @name multiple_phenotypes_impl
NULL

#' Calculate Multiple PheProbs using Original Binomial Mixture Models
#'
#' Implements multiple phenotype analysis using the PheProb methodology (Sinnott et al., 2018).
#' Each phenotype gets its own binomial mixture model fitted to (S_k, C) pairs
#' where S_k is the count of codes relevant to phenotype k and C is total
#' healthcare utilization.
#'
#' @param phenotype_concepts A named list where each element contains the concept IDs
#'   for a specific phenotype
#' @param person_ids A numeric vector of person IDs. If NULL, includes all available persons.
#' @param domains A character vector specifying which OMOP domains to search
#' @param date_range Optional list with 'start' and 'end' dates to limit temporal scope
#' @param output_format Character string specifying output format: "wide", "long"
#' @param output_file Optional file path to save results
#' @param batch_size Integer specifying batch size for processing large datasets
#' @param progress Logical indicating whether to show progress bars
#' @param max_iterations Maximum EM iterations for each binomial mixture model
#' @param convergence_threshold Convergence threshold for EM algorithm
#' @param init_method Initialization method for EM: "random", "kmeans", "manual"
#' @param exclude_concepts Concept IDs to exclude from total healthcare utilization counts
#' @param data_validation Logical indicating whether to perform data quality validation
#' @param model_diagnostics Logical indicating whether to include model diagnostics
#' @param phenotype_correlation_analysis Logical indicating whether to analyze correlations
#' @param joint_validation Logical indicating whether to perform joint validation
#' @param ... Additional arguments
#'
#' @return A tibble with class "pheprob_multiple" containing multiple phenotype
#'   probabilities and analysis results
#'
#' @export
calculate_multiple_pheprobs_method <- function(phenotype_concepts,
                                               person_ids = NULL,
                                               domains = c("condition", "procedure", "drug", "measurement", "observation"),
                                               date_range = NULL,
                                               output_format = "wide",
                                               output_file = NULL,
                                               batch_size = 10000,
                                               progress = TRUE,
                                               max_iterations = 1000,
                                               convergence_threshold = 1e-6,
                                               init_method = "random",
                                               exclude_concepts = NULL,
                                               data_validation = TRUE,
                                               model_diagnostics = TRUE,
                                               phenotype_correlation_analysis = TRUE,
                                               joint_validation = TRUE,
                                               ...) {
  
  start_time <- Sys.time()
  
  # Step 0: Establish single database connection first (reused for all operations)
  if (progress) cli::cli_progress_step("Establishing database connection...")
  
  # Ensure allofus package is available
  if (!requireNamespace("allofus", quietly = TRUE)) {
    cli::cli_abort("allofus package is required but not available")
  }
  
  # Connect once and reuse for all operations
  con <- allofus::aou_connect()
  
  # Input validation (with shared connection for database-dependent validations)
  if (progress) cli::cli_progress_step("Validating inputs...")
  
  validated_phenotypes <- validate_phenotype_coherence(phenotype_concepts, con = con)
  
  person_validation <- validate_person_ids(person_ids)
  validated_person_ids <- person_validation$validated_person_ids
  
  validated_domains <- validate_domains(domains)
  
  phenotype_names <- names(validated_phenotypes$concepts)
  n_phenotypes <- length(phenotype_names)
  
  if (progress) {
    cli::cli_alert_info("Processing {n_phenotypes} phenotypes using PheProb methodology (Sinnott et al., 2018)")
    cli::cli_alert_info("Phenotypes: {paste(phenotype_names, collapse = ', ')}")
  }
  
  # Step 0.1: We'll derive total healthcare utilization from the first phenotype
  # This avoids BigQuery compatibility issues with the separate extraction function
  total_utilization <- NULL
  
  # Step 1: Process each phenotype using shared connection
  if (progress) cli::cli_progress_step("Processing individual phenotypes with concept hierarchy expansion...")
  
  phenotype_results <- list()
  phenotype_models <- list()
  phenotype_validations <- list()
  
  if (progress) {
    cli::cli_progress_bar("Phenotype processing", total = n_phenotypes)
  }
  
  for (i in seq_along(phenotype_names)) {
    phenotype_name <- phenotype_names[i]
    concept_ids <- validated_phenotypes$concepts[[phenotype_name]]
    
    if (progress) {
      cli::cli_progress_update(set = i)
      cli::cli_alert_info("Processing {phenotype_name} ({i}/{n_phenotypes}) with concept hierarchy expansion...")
    }
    
    tryCatch({
      # Use the improved data extraction with shared connection
      phenotype_data <- extract_allofus_pheprob_data(
        concept_ids = concept_ids,
        person_ids = validated_person_ids,
        domains = validated_domains,
        date_range = date_range,
        expand_concepts = TRUE,  # Enable concept hierarchy expansion
        max_persons = NULL,
        con = con  # Pass the shared connection
      )
      
      # Data validation for this phenotype
      phenotype_validation <- NULL
      if (data_validation) {
        phenotype_validation <- validate_binomial_data_quality(
          phenotype_data, 
          concept_ids = concept_ids,
          phenotype_name = phenotype_name
        )
        phenotype_validations[[phenotype_name]] <- phenotype_validation
      }
      
      # Fit binomial mixture model
      mixture_model <- fit_pheprob_binomial_mixture(
        S = phenotype_data$S,
        C = phenotype_data$C,
        max_iterations = max_iterations,
        convergence_threshold = convergence_threshold,
        verbose = FALSE,  # Suppress individual model progress
        init_method = init_method,
        phenotype_name = phenotype_name
      )
      
      # Store model for later analysis
      if (model_diagnostics) {
        phenotype_models[[phenotype_name]] <- mixture_model
      }
      
      # Create results for this phenotype
      phenotype_result <- phenotype_data %>%
        dplyr::select(.data$person_id, .data$C, .data$S, .data$success_rate) %>%
        dplyr::mutate(
          !!paste0(phenotype_name, "_prob") := mixture_model$phenotype_probabilities,
          !!paste0(phenotype_name, "_case_posterior") := mixture_model$posterior_probabilities[, "case"],
          !!paste0(phenotype_name, "_control_posterior") := mixture_model$posterior_probabilities[, "control"]
        )
      
      phenotype_results[[phenotype_name]] <- phenotype_result
      
      if (progress) {
        mean_prob <- round(mean(mixture_model$phenotype_probabilities), 3)
        n_high_conf <- sum(mixture_model$phenotype_probabilities > 0.8)
        convergence_status <- ifelse(mixture_model$convergence, "✓", "✗")
        cli::cli_alert_success("{convergence_status} {phenotype_name}: mean prob = {mean_prob}, high conf = {n_high_conf}")
      }
      
    }, error = function(e) {
      if (progress) {
        cli::cli_alert_warning("✗ Error processing {phenotype_name}: {e$message}")
      }
      
      # For failed phenotypes, we'll set them to NULL and handle in the combine step
      phenotype_results[[phenotype_name]] <- NULL
      phenotype_models[[phenotype_name]] <- NULL
      phenotype_validations[[phenotype_name]] <- list(
        overall_quality = list(score = 0, interpretation = "Failed"),
        warnings = c("Phenotype processing failed")
      )
    })
  }
  
  if (progress) {
    cli::cli_progress_done()
  }
  
  # Step 1.5: Derive total utilization from first successful phenotype
  if (progress) cli::cli_progress_step("Deriving total healthcare utilization from phenotype results...")
  
  # Find the first successful phenotype result to use as base for total utilization
  total_utilization <- NULL
  for (phenotype_name in phenotype_names) {
    if (phenotype_name %in% names(phenotype_results) && !is.null(phenotype_results[[phenotype_name]])) {
      phenotype_data <- phenotype_results[[phenotype_name]]
      total_utilization <- phenotype_data %>%
        dplyr::select(.data$person_id, 
                     total_code_count = .data$C,
                     .data$first_code_date, 
                     .data$last_code_date, 
                     .data$healthcare_span_days) %>%
        dplyr::distinct(.data$person_id, .keep_all = TRUE)  # Ensure unique persons
      break
    }
  }
  
  if (is.null(total_utilization)) {
    cli::cli_abort("No successful phenotype extractions - cannot derive total healthcare utilization")
  }
  
  # Step 2: Combine results across phenotypes
  if (progress) cli::cli_progress_step("Combining results across phenotypes...")
  
  combined_results <- combine_multiple_phenotype_results_original(
    phenotype_results, 
    total_utilization,
    phenotype_names
  )
  
  # Debug: Check probability columns
  if (progress) {
    prob_cols <- paste0(phenotype_names, "_prob")
    existing_prob_cols <- prob_cols[prob_cols %in% names(combined_results)]
    if (length(existing_prob_cols) > 0) {
      cli::cli_alert_info("Found probability columns: {paste(existing_prob_cols, collapse = ', ')}")
      for (col in existing_prob_cols) {
        vals <- combined_results[[col]]
        non_na_vals <- vals[!is.na(vals)]
        if (length(non_na_vals) > 0) {
          cli::cli_alert_info("{col}: {length(non_na_vals)} non-NA values, range: {round(min(non_na_vals), 3)} - {round(max(non_na_vals), 3)}")
        } else {
          cli::cli_alert_warning("{col}: All values are NA")
        }
      }
    } else {
      cli::cli_alert_warning("No probability columns found in combined results")
      cli::cli_alert_info("Available columns: {paste(names(combined_results), collapse = ', ')}")
    }
  }
  
  # Step 3: Phenotype correlation analysis
  correlation_analysis <- NULL
  if (phenotype_correlation_analysis && n_phenotypes > 1) {
    if (progress) cli::cli_progress_step("Analyzing phenotype correlations...")
    
    tryCatch({
      correlation_analysis <- analyze_phenotype_correlations_original(
        combined_results, 
        phenotype_names
      )
    }, error = function(e) {
      if (progress) {
        cli::cli_alert_warning("Correlation analysis failed: {e$message}")
        cli::cli_alert_info("Continuing without correlation analysis...")
      }
      correlation_analysis <<- NULL
    })
  }
  
  # Step 4: Joint validation across phenotypes
  joint_validation_results <- NULL
  if (joint_validation && n_phenotypes > 1) {
    if (progress) cli::cli_progress_step("Performing joint validation...")
    
    joint_validation_results <- perform_joint_validation_original(
      combined_results,
      phenotype_models,
      phenotype_validations,
      phenotype_names
    )
  }
  
  # Step 5: Create comprehensive metadata
  end_time <- Sys.time()
  
  metadata <- list(
    method = "multiple_pheprob",
    phenotype_names = phenotype_names,
    n_phenotypes = n_phenotypes,
    phenotype_concepts = validated_phenotypes$concepts,
    domains = validated_domains,
    date_range = date_range,
    n_patients = nrow(combined_results),
    calculation_time = end_time - start_time,
    convergence_summary = summarize_convergence(phenotype_models),
    package_version = utils::packageVersion("pheprobAoU")
  )
  
  # Add optional components
  if (model_diagnostics) {
    metadata$model_diagnostics <- phenotype_models
  }
  
  if (data_validation) {
    metadata$individual_validations <- phenotype_validations
  }
  
  if (!is.null(correlation_analysis)) {
    metadata$correlation_analysis <- correlation_analysis
  }
  
  if (!is.null(joint_validation_results)) {
    metadata$joint_validation <- joint_validation_results
  }
  
  # Step 6: Format output
  formatted_output <- format_multiple_original_output(
    combined_results,
    output_format,
    metadata
  )
  
  # Step 7: Save to file if requested
  if (!is.null(output_file)) {
    if (progress) cli::cli_progress_step("Saving results to file...")
    export_multiple_original_results(formatted_output, output_file)
  }
  
  # Final summary
  if (progress) {
    processing_time <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 2)
    
    cli::cli_alert_success("Multiple phenotype analysis complete!")
    cli::cli_alert_info("Processing time: {processing_time} seconds")
    cli::cli_alert_info("Analyzed {n_phenotypes} phenotypes for {nrow(combined_results)} patients")
    
    # Convergence summary
    convergence_count <- sum(sapply(phenotype_models, function(x) if (is.null(x)) FALSE else x$convergence))
    cli::cli_alert_info("Model convergence: {convergence_count}/{n_phenotypes} phenotypes")
    
    # Overall probability ranges
    prob_cols <- paste0(phenotype_names, "_prob")
    for (col in prob_cols) {
      if (col %in% names(combined_results)) {
        probs <- combined_results[[col]]
        phenotype_name <- sub("_prob$", "", col)
        mean_prob <- round(mean(probs), 3)
        high_conf <- sum(probs > 0.8)
        cli::cli_text("  • {phenotype_name}: mean = {mean_prob}, high confidence = {high_conf}")
      }
    }
  }
  
  return(formatted_output)
}

#' Combine Multiple Phenotype Results (Original Method)
#'
#' Combines individual phenotype results into a unified dataset.
#'
#' @param phenotype_results List of individual phenotype result tibbles
#' @param total_utilization Total healthcare utilization data
#' @param phenotype_names Vector of phenotype names
#'
#' @return Combined results tibble
#'
#' @keywords internal
combine_multiple_phenotype_results_original <- function(phenotype_results, 
                                                       total_utilization,
                                                       phenotype_names) {
  
  # Start with total utilization as base
  combined <- total_utilization %>%
    dplyr::select(.data$person_id, total_codes = .data$total_code_count,
                 .data$first_code_date, .data$last_code_date, .data$healthcare_span_days)
  
  # Add each phenotype's results
  for (phenotype_name in phenotype_names) {
    if (phenotype_name %in% names(phenotype_results) && !is.null(phenotype_results[[phenotype_name]])) {
      phenotype_data <- phenotype_results[[phenotype_name]] %>%
        dplyr::select(.data$person_id, 
                     !!paste0(phenotype_name, "_relevant_codes") := .data$S,
                     !!paste0(phenotype_name, "_success_rate") := .data$success_rate,
                     !!paste0(phenotype_name, "_prob") := !!paste0(phenotype_name, "_prob"),
                     !!paste0(phenotype_name, "_case_posterior") := !!paste0(phenotype_name, "_case_posterior"),
                     !!paste0(phenotype_name, "_control_posterior") := !!paste0(phenotype_name, "_control_posterior"))
      
      combined <- combined %>%
        dplyr::left_join(phenotype_data, by = "person_id")
    } else {
      # Add zero columns for failed phenotypes
      combined <- combined %>%
        dplyr::mutate(
          !!paste0(phenotype_name, "_relevant_codes") := 0L,
          !!paste0(phenotype_name, "_success_rate") := 0,
          !!paste0(phenotype_name, "_prob") := 0,
          !!paste0(phenotype_name, "_case_posterior") := 0,
          !!paste0(phenotype_name, "_control_posterior") := 1
        )
    }
  }
  
  # Fill missing values with 0 for probability columns
  prob_cols <- names(combined)[grepl("_(prob|posterior|relevant_codes|success_rate)$", names(combined))]
  combined[prob_cols] <- lapply(combined[prob_cols], function(x) ifelse(is.na(x), 0, x))
  
  return(combined)
}

#' Analyze Phenotype Correlations (Original Method)
#'
#' Analyzes correlations between multiple phenotype probabilities.
#'
#' @param combined_results Combined results tibble
#' @param phenotype_names Vector of phenotype names
#'
#' @return Correlation analysis results
#'
#' @keywords internal
analyze_phenotype_correlations_original <- function(combined_results, phenotype_names) {
  
  # Extract probability columns
  prob_cols <- paste0(phenotype_names, "_prob")
  
  # Check if probability columns exist
  missing_cols <- prob_cols[!prob_cols %in% names(combined_results)]
  if (length(missing_cols) > 0) {
    cli::cli_abort("Missing probability columns in combined results: {paste(missing_cols, collapse = ', ')}")
  }
  
  prob_data <- combined_results[prob_cols]
  
  # Check if there's any non-NA data
  if (all(is.na(prob_data))) {
    cli::cli_abort("All probability data is NA - cannot calculate correlations")
  }
  
  # Check if any columns have non-zero variance
  non_constant_cols <- sapply(prob_data, function(x) {
    x_clean <- x[!is.na(x)]
    length(unique(x_clean)) > 1
  })
  
  if (!any(non_constant_cols)) {
    cli::cli_alert_warning("All probability columns are constant - skipping correlation analysis")
    return(list(
      correlation_matrix = NULL,
      pairwise_correlations = list(),
      strong_correlations = list(),
      comorbidity_patterns = analyze_comorbidity_patterns(combined_results, phenotype_names),
      summary = list(
        n_phenotypes = length(phenotype_names),
        n_pairs_tested = 0,
        n_strong_correlations = 0,
        max_correlation = NA,
        mean_correlation = NA,
        warning = "All probability columns are constant"
      )
    ))
  }
  
  # Calculate correlation matrix
  correlation_matrix <- cor(prob_data, use = "complete.obs")
  
  # Calculate pairwise correlations with significance tests
  pairwise_correlations <- list()
  n_pairs <- 0
  
  for (i in 1:(length(phenotype_names)-1)) {
    for (j in (i+1):length(phenotype_names)) {
      pheno1 <- phenotype_names[i]
      pheno2 <- phenotype_names[j]
      
      x <- combined_results[[paste0(pheno1, "_prob")]]
      y <- combined_results[[paste0(pheno2, "_prob")]]
      
      # Remove missing values
      complete_cases <- complete.cases(x, y)
      x_clean <- x[complete_cases]
      y_clean <- y[complete_cases]
      
      if (length(x_clean) > 10) {  # Minimum sample size
        cor_test <- cor.test(x_clean, y_clean)
        
        pairwise_correlations[[paste0(pheno1, "_vs_", pheno2)]] <- list(
          phenotype1 = pheno1,
          phenotype2 = pheno2,
          correlation = cor_test$estimate,
          p_value = cor_test$p.value,
          ci_lower = cor_test$conf.int[1],
          ci_upper = cor_test$conf.int[2],
          n_observations = length(x_clean)
        )
        
        n_pairs <- n_pairs + 1
      }
    }
  }
  
  # Identify strong correlations
  strong_correlations <- list()
  for (pair_name in names(pairwise_correlations)) {
    pair_data <- pairwise_correlations[[pair_name]]
    if (abs(pair_data$correlation) > 0.3 && pair_data$p_value < 0.05) {
      strong_correlations[[pair_name]] <- pair_data
    }
  }
  
  # Comorbidity analysis (patients with multiple high-probability phenotypes)
  comorbidity_analysis <- analyze_comorbidity_patterns(combined_results, phenotype_names)
  
  return(list(
    correlation_matrix = correlation_matrix,
    pairwise_correlations = pairwise_correlations,
    strong_correlations = strong_correlations,
    comorbidity_patterns = comorbidity_analysis,
    summary = list(
      n_phenotypes = length(phenotype_names),
      n_pairs_tested = n_pairs,
      n_strong_correlations = length(strong_correlations),
      max_correlation = max(abs(correlation_matrix[upper.tri(correlation_matrix)])),
      mean_correlation = mean(abs(correlation_matrix[upper.tri(correlation_matrix)]))
    )
  ))
}

#' Analyze Comorbidity Patterns
#'
#' Analyzes patterns of comorbidity (multiple high-probability phenotypes).
#'
#' @param combined_results Combined results tibble
#' @param phenotype_names Vector of phenotype names
#' @param threshold Probability threshold for "high probability" (default: 0.8)
#'
#' @return Comorbidity analysis results
#'
#' @keywords internal
analyze_comorbidity_patterns <- function(combined_results, phenotype_names, threshold = 0.8) {
  
  # Create binary high-probability indicators
  prob_cols <- paste0(phenotype_names, "_prob")
  high_prob_data <- combined_results[prob_cols] > threshold
  names(high_prob_data) <- phenotype_names
  
  # Calculate number of high-probability phenotypes per patient
  n_high_prob <- rowSums(high_prob_data, na.rm = TRUE)
  
  # Comorbidity distribution
  comorbidity_distribution <- table(n_high_prob)
  
  # Common comorbidity pairs
  comorbidity_pairs <- list()
  
  for (i in 1:(length(phenotype_names)-1)) {
    for (j in (i+1):length(phenotype_names)) {
      pheno1 <- phenotype_names[i]
      pheno2 <- phenotype_names[j]
      
      both_high <- high_prob_data[[pheno1]] & high_prob_data[[pheno2]]
      n_both <- sum(both_high, na.rm = TRUE)
      
      if (n_both > 0) {
        comorbidity_pairs[[paste0(pheno1, "_", pheno2)]] <- list(
          phenotype1 = pheno1,
          phenotype2 = pheno2,
          n_comorbid = n_both,
          rate_comorbid = n_both / nrow(combined_results),
          patient_ids = combined_results$person_id[both_high]
        )
      }
    }
  }
  
  # Sort by frequency
  comorbidity_pairs <- comorbidity_pairs[order(sapply(comorbidity_pairs, function(x) x$n_comorbid), decreasing = TRUE)]
  
  return(list(
    threshold_used = threshold,
    comorbidity_distribution = comorbidity_distribution,
    mean_phenotypes_per_patient = mean(n_high_prob, na.rm = TRUE),
    patients_with_multiple = sum(n_high_prob > 1, na.rm = TRUE),
    most_common_pairs = head(comorbidity_pairs, 5),
    all_pairs = comorbidity_pairs
  ))
}

#' Perform Joint Validation (Original Method)
#'
#' Performs validation across multiple phenotypes.
#'
#' @param combined_results Combined results tibble
#' @param phenotype_models List of fitted models
#' @param phenotype_validations List of individual validations
#' @param phenotype_names Vector of phenotype names
#'
#' @return Joint validation results
#'
#' @keywords internal
perform_joint_validation_original <- function(combined_results,
                                             phenotype_models,
                                             phenotype_validations,
                                             phenotype_names) {
  
  # Cross-phenotype consistency checks
  consistency_checks <- list()
  
  # Check for consistent healthcare utilization effects
  alpha_1_values <- sapply(phenotype_models, function(model) {
    if (is.null(model)) return(NA)
    model$parameters$alpha_1
  })
  
  consistency_checks$utilization_effects <- list(
    alpha_1_values = alpha_1_values,
    alpha_1_range = range(alpha_1_values, na.rm = TRUE),
    alpha_1_consistency = sd(alpha_1_values, na.rm = TRUE) < 0.1  # Arbitrary threshold
  )
  
  # Check for reasonable parameter ranges
  p_1_values <- sapply(phenotype_models, function(model) {
    if (is.null(model)) return(NA)
    model$parameters$p_1
  })
  
  p_0_values <- sapply(phenotype_models, function(model) {
    if (is.null(model)) return(NA)
    model$parameters$p_0
  })
  
  consistency_checks$success_probabilities <- list(
    p_1_range = range(p_1_values, na.rm = TRUE),
    p_0_range = range(p_0_values, na.rm = TRUE),
    separation_range = range(p_1_values - p_0_values, na.rm = TRUE),
    reasonable_separation = all((p_1_values - p_0_values) > 0.05, na.rm = TRUE)
  )
  
  # Overall data quality across phenotypes
  overall_quality_scores <- sapply(phenotype_validations, function(val) {
    if (is.null(val) || is.null(val$overall_quality)) return(0)
    val$overall_quality$score
  })
  
  # Cross-phenotype warnings
  cross_warnings <- character(0)
  
  if (any(overall_quality_scores < 60)) {
    low_quality <- phenotype_names[overall_quality_scores < 60]
    cross_warnings <- c(cross_warnings, 
                       paste("Low data quality for phenotypes:", paste(low_quality, collapse = ", ")))
  }
  
  if (!consistency_checks$utilization_effects$alpha_1_consistency) {
    cross_warnings <- c(cross_warnings, 
                       "Inconsistent healthcare utilization effects across phenotypes")
  }
  
  if (!consistency_checks$success_probabilities$reasonable_separation) {
    cross_warnings <- c(cross_warnings,
                       "Poor case/control separation for some phenotypes")
  }
  
  return(list(
    consistency_checks = consistency_checks,
    overall_quality_scores = overall_quality_scores,
    mean_quality_score = mean(overall_quality_scores, na.rm = TRUE),
    cross_phenotype_warnings = cross_warnings,
    validation_summary = list(
      n_phenotypes_validated = sum(!is.na(overall_quality_scores)),
      n_high_quality = sum(overall_quality_scores >= 80, na.rm = TRUE),
      n_warnings = length(cross_warnings)
    )
  ))
}

#' Summarize Convergence Across Models
#'
#' Summarizes EM convergence across multiple phenotype models.
#'
#' @param phenotype_models List of fitted models
#'
#' @return Convergence summary
#'
#' @keywords internal
summarize_convergence <- function(phenotype_models) {
  
  convergence_info <- lapply(phenotype_models, function(model) {
    if (is.null(model)) {
      return(list(converged = FALSE, iterations = NA, log_likelihood = NA))
    }
    
    list(
      converged = model$convergence,
      iterations = model$n_iterations,
      log_likelihood = model$log_likelihood
    )
  })
  
  converged_count <- sum(sapply(convergence_info, function(x) x$converged), na.rm = TRUE)
  total_count <- length(convergence_info)
  
  return(list(
    individual_convergence = convergence_info,
    convergence_rate = converged_count / total_count,
    converged_count = converged_count,
    total_count = total_count,
    mean_iterations = mean(sapply(convergence_info, function(x) x$iterations), na.rm = TRUE)
  ))
}

#' Format Multiple Original Output
#'
#' Formats the output from multiple original PheProb analysis.
#'
#' @param combined_results Combined results tibble
#' @param output_format Requested output format
#' @param metadata Metadata list
#'
#' @return Formatted output with appropriate class and attributes
#'
#' @keywords internal
format_multiple_original_output <- function(combined_results, output_format, metadata) {
  
  if (output_format == "wide") {
    # Default wide format
    formatted_output <- combined_results
    
  } else if (output_format == "long") {
    # Long format - pivot probabilities
    prob_cols <- names(combined_results)[grepl("_prob$", names(combined_results))]
    
    formatted_output <- combined_results %>%
      dplyr::select(.data$person_id, .data$total_codes, .data$first_code_date, 
                   .data$last_code_date, .data$healthcare_span_days, dplyr::all_of(prob_cols)) %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(prob_cols),
        names_to = "phenotype_name",
        values_to = "pheprob_score"
      ) %>%
      dplyr::mutate(
        phenotype_name = stringr::str_remove(.data$phenotype_name, "_prob$")
      )
    
  } else {
    cli::cli_abort("Unknown output_format: {output_format}. Must be one of: wide, long")
  }
  
  # Add class and attributes
  class(formatted_output) <- c("pheprob_multiple", "data.frame")
  
  # Add metadata as attributes
  for (attr_name in names(metadata)) {
    attr(formatted_output, attr_name) <- metadata[[attr_name]]
  }
  
  return(formatted_output)
}

#' Export Multiple Original Results
#'
#' Exports multiple original PheProb results to specified file format.
#'
#' @param results Formatted results from multiple original PheProb
#' @param output_file File path for export
#'
#' @keywords internal
export_multiple_original_results <- function(results, output_file) {
  
  file_ext <- tools::file_ext(output_file)
  
  tryCatch({
    if (file_ext == "csv") {
      # CSV export - main results only
      readr::write_csv(as.data.frame(results), output_file)
      
    } else if (file_ext == "rds") {
      # RDS export - preserve full object with metadata
      saveRDS(results, output_file)
      
    } else if (file_ext == "xlsx") {
      # Excel export with multiple sheets
      if (requireNamespace("writexl", quietly = TRUE)) {
        
        # Main results
        export_data <- list("Results" = as.data.frame(results))
        
        # Add correlation analysis if available
        if (!is.null(attr(results, "correlation_analysis"))) {
          corr_matrix <- attr(results, "correlation_analysis")$correlation_matrix
          export_data$Correlations <- as.data.frame(corr_matrix)
        }
        
        # Add validation summary if available
        if (!is.null(attr(results, "joint_validation"))) {
          validation_summary <- data.frame(
            Phenotype = names(attr(results, "joint_validation")$overall_quality_scores),
            Quality_Score = attr(results, "joint_validation")$overall_quality_scores
          )
          export_data$Validation <- validation_summary
        }
        
        writexl::write_xlsx(export_data, output_file)
      } else {
        cli::cli_alert_warning("writexl package not available. Saving as CSV instead.")
        new_file <- sub("\\.xlsx$", ".csv", output_file)
        export_multiple_original_results(results, new_file)
      }
      
    } else {
      cli::cli_alert_warning("Unknown file extension: {file_ext}. Saving as RDS.")
      new_file <- sub("\\.[^.]*$", ".rds", output_file)
      saveRDS(results, new_file)
    }
    
    cli::cli_alert_success("Multiple phenotype results saved to: {output_file}")
    
  }, error = function(e) {
    cli::cli_alert_warning("Failed to save results: {e$message}")
  })
}
