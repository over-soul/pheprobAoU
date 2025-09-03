#' PheProb Implementation (Sinnott et al., 2018)
#'
#' This file implements the PheProb methodology using binomial mixture models
#' as described in Sinnott et al. (2018). This is the main implementation that should be
#' used for proper probabilistic phenotyping.
#'
#' @name single_phenotype_impl
NULL

#' Calculate PheProb using Original Binomial Mixture Model
#'
#' Implements the original PheProb algorithm using a binomial mixture model with
#' healthcare utilization adjustment. This provides true phenotype probabilities
#' P(Y=1|S,C) where S is disease-relevant code count and C is total healthcare utilization.
#'
#' @param concept_ids A numeric vector of disease-relevant OMOP concept IDs
#' @param person_ids A numeric vector of person IDs. If NULL, includes all available persons.
#' @param domains A character vector specifying which OMOP domains to search.
#' @param date_range Optional list with 'start' and 'end' dates (Date objects) to limit
#'   the temporal scope of data extraction.
#' @param output_format Character string specifying output format: "wide", "long", "matrix"
#' @param output_file Optional file path to save results
#' @param batch_size Integer specifying batch size for processing large datasets
#' @param progress Logical indicating whether to show progress bars
#' @param max_iterations Maximum EM iterations for binomial mixture model
#' @param convergence_threshold Convergence threshold for EM algorithm
#' @param init_method Initialization method for EM: "random", "kmeans", "manual"
#' @param exclude_concepts Concept IDs to exclude from total healthcare utilization counts
#' @param data_validation Logical indicating whether to perform data quality validation
#' @param model_diagnostics Logical indicating whether to include model diagnostics
#' @param check_concept_existence Logical indicating whether to validate concept IDs exist in database (default: FALSE)
#' @param ... Additional arguments
#'
#' @return A tibble with class "pheprob_single" containing phenotype probabilities
#'   and model information
#'
#' @export
calculate_pheprob_method <- function(concept_ids,
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
                                            check_concept_existence = FALSE,
                                            ...) {
  
  start_time <- Sys.time()
  
  # Input validation
  if (progress) cli::cli_progress_step("Validating inputs...")
  
  concept_validation <- validate_concept_ids(concept_ids, check_existence = check_concept_existence)
  validated_concept_ids <- concept_validation$validated_concept_ids
  
  person_validation <- validate_person_ids(person_ids)
  validated_person_ids <- person_validation$validated_person_ids
  
  validated_domains <- validate_domains(domains)
  
  # Step 1: Prepare binomial mixture data (S, C pairs)
  if (progress) cli::cli_progress_step("Extracting and preparing data...")
  
  pheprob_data <- prepare_pheprob_binomial_data(
    concept_ids = validated_concept_ids,
    person_ids = validated_person_ids,
    domains = validated_domains,
    date_range = date_range,
    exclude_concepts = exclude_concepts,
    max_persons = NULL  # Process all requested persons
  )
  
  if (nrow(pheprob_data) == 0) {
    cli::cli_abort("No data available for the specified parameters")
  }
  
  # Step 2: Data quality validation
  validation_results <- NULL
  if (data_validation) {
    if (progress) cli::cli_progress_step("Validating data quality...")
    
    validation_results <- validate_binomial_data_quality(
      pheprob_data, 
      concept_ids = validated_concept_ids
    )
    
    if (validation_results$overall_quality$score < 60) {
      cli::cli_alert_warning("Data quality score: {round(validation_results$overall_quality$score, 1)}/100 ({validation_results$overall_quality$interpretation})")
      cli::cli_alert_warning("Recommendation: {validation_results$overall_quality$recommendation}")
      
      if (length(validation_results$warnings) > 0) {
        cli::cli_alert_warning("Data quality issues:")
        for (warning in validation_results$warnings) {
          cli::cli_text("  • {warning}")
        }
      }
    } else if (progress) {
      cli::cli_alert_success("Data quality: {validation_results$overall_quality$interpretation} ({round(validation_results$overall_quality$score, 1)}/100)")
    }
  }
  
  # Step 3: Fit binomial mixture model
  if (progress) cli::cli_progress_step("Fitting binomial mixture model...")
  
  mixture_model <- fit_pheprob_binomial_mixture(
    S = pheprob_data$S,
    C = pheprob_data$C,
    max_iterations = max_iterations,
    convergence_threshold = convergence_threshold,
    verbose = progress,
    init_method = init_method
  )
  
  if (!mixture_model$convergence && progress) {
    cli::cli_alert_warning("EM algorithm did not converge. Results may be unreliable.")
  }
  
  # Step 4: Create results tibble
  if (progress) cli::cli_progress_step("Formatting results...")
  
  results_tibble <- pheprob_data %>%
    dplyr::mutate(
      pheprob_score = mixture_model$phenotype_probabilities,
      case_probability = mixture_model$posterior_probabilities[, "case"],
      control_probability = mixture_model$posterior_probabilities[, "control"]
    ) %>%
    dplyr::select(
      .data$person_id,
      .data$pheprob_score,
      total_codes = .data$C,
      relevant_codes = .data$S,
      .data$success_rate,
      .data$case_probability,
      .data$control_probability,
      .data$first_code_date,
      .data$last_code_date,
      .data$healthcare_span_days
    )
  
  # Step 5: Add model metadata and diagnostics
  model_metadata <- list(
    method = "single_pheprob",
    concept_ids = validated_concept_ids,
    n_concepts = length(validated_concept_ids),
    domains = validated_domains,
    date_range = date_range,
    n_patients = nrow(results_tibble),
    calculation_time = Sys.time() - start_time,
    convergence = mixture_model$convergence,
    n_iterations = mixture_model$n_iterations,
    log_likelihood = mixture_model$log_likelihood,
    parameters = mixture_model$parameters,
    package_version = utils::packageVersion("pheprobAoU")
  )
  
  # Add validation results if performed
  if (!is.null(validation_results)) {
    model_metadata$data_validation <- validation_results
  }
  
  # Add diagnostics if requested
  if (model_diagnostics) {
    model_metadata$diagnostics <- mixture_model$model_diagnostics
  }
  
  # Step 6: Format output according to requested format
  formatted_output <- format_original_pheprob_output(
    results_tibble, 
    output_format,
    model_metadata
  )
  
  # Step 7: Save to file if requested
  if (!is.null(output_file)) {
    if (progress) cli::cli_progress_step("Saving results to file...")
    export_original_pheprob_results(formatted_output, output_file)
  }
  
  # Final summary
  if (progress) {
    end_time <- Sys.time()
    processing_time <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 2)
    
    cli::cli_alert_success("Single PheProb calculation complete!")
    cli::cli_alert_info("Processing time: {processing_time} seconds")
    cli::cli_alert_info("Phenotype probabilities calculated for {nrow(results_tibble)} patients")
    
    # Summary statistics
    probs <- results_tibble$pheprob_score
    cli::cli_alert_info("Probability summary:")
    cli::cli_text("  • Mean: {round(mean(probs), 3)}")
    cli::cli_text("  • Range: {round(min(probs), 3)} - {round(max(probs), 3)}")
    cli::cli_text("  • High confidence cases (>0.8): {sum(probs > 0.8)} ({round(100*mean(probs > 0.8), 1)}%)")
    cli::cli_text("  • High confidence controls (<0.2): {sum(probs < 0.2)} ({round(100*mean(probs < 0.2), 1)}%)")
  }
  
  return(formatted_output)
}

#' Format Output for Original PheProb Results
#'
#' Formats the results from original PheProb calculation according to requested format.
#'
#' @param results_tibble Tibble with calculation results
#' @param output_format Requested output format
#' @param model_metadata Model metadata and diagnostics
#'
#' @return Formatted results with appropriate class and attributes
#'
#' @keywords internal
format_original_pheprob_output <- function(results_tibble, output_format, model_metadata) {
  
  if (output_format == "wide") {
    # Default wide format - return tibble as-is
    formatted_output <- results_tibble
    
  } else if (output_format == "long") {
    # Long format - reshape for detailed analysis
    formatted_output <- results_tibble %>%
      tidyr::pivot_longer(
        cols = c(.data$pheprob_score, .data$case_probability, .data$control_probability),
        names_to = "probability_type",
        values_to = "probability_value"
      ) %>%
      dplyr::mutate(
        probability_type = dplyr::case_when(
          .data$probability_type == "pheprob_score" ~ "phenotype_probability",
          .data$probability_type == "case_probability" ~ "case_posterior",
          .data$probability_type == "control_probability" ~ "control_posterior",
          TRUE ~ .data$probability_type
        )
      )
    
  } else if (output_format == "matrix") {
    # Matrix format - person x probability matrix
    formatted_output <- as.matrix(results_tibble %>%
      dplyr::select(.data$pheprob_score, .data$case_probability, .data$control_probability))
    rownames(formatted_output) <- results_tibble$person_id
    colnames(formatted_output) <- c("pheprob_score", "case_probability", "control_probability")
    
  } else {
    cli::cli_abort("Unknown output_format: {output_format}. Must be one of: wide, long, matrix")
  }
  
  # Add class and attributes
  if (!is.matrix(formatted_output)) {
    class(formatted_output) <- c("pheprob_single", "pheprob", "data.frame")
  }
  
  # Add metadata as attributes
  for (attr_name in names(model_metadata)) {
    attr(formatted_output, attr_name) <- model_metadata[[attr_name]]
  }
  
  return(formatted_output)
}

#' Export Original PheProb Results
#'
#' Exports original PheProb results to specified file format.
#'
#' @param results Formatted results from original PheProb
#' @param output_file File path for export
#'
#' @keywords internal
export_original_pheprob_results <- function(results, output_file) {
  
  file_ext <- tools::file_ext(output_file)
  
  tryCatch({
    if (file_ext == "csv") {
      # CSV export - flatten to data frame
      if (is.matrix(results)) {
        export_data <- as.data.frame(results)
        export_data$person_id <- rownames(results)
        export_data <- export_data[, c("person_id", setdiff(names(export_data), "person_id"))]
      } else {
        export_data <- as.data.frame(results)
      }
      readr::write_csv(export_data, output_file)
      
    } else if (file_ext == "rds") {
      # RDS export - preserve full object with metadata
      saveRDS(results, output_file)
      
    } else if (file_ext == "xlsx") {
      # Excel export
      if (requireNamespace("writexl", quietly = TRUE)) {
        if (is.matrix(results)) {
          export_data <- as.data.frame(results)
          export_data$person_id <- rownames(results)
          export_data <- export_data[, c("person_id", setdiff(names(export_data), "person_id"))]
        } else {
          export_data <- as.data.frame(results)
        }
        writexl::write_xlsx(export_data, output_file)
      } else {
        cli::cli_alert_warning("writexl package not available. Saving as CSV instead.")
        new_file <- sub("\\.xlsx$", ".csv", output_file)
        export_original_pheprob_results(results, new_file)
      }
      
    } else {
      cli::cli_alert_warning("Unknown file extension: {file_ext}. Saving as RDS.")
      new_file <- sub("\\.[^.]*$", ".rds", output_file)
      saveRDS(results, new_file)
    }
    
    cli::cli_alert_success("Results saved to: {output_file}")
    
  }, error = function(e) {
    cli::cli_alert_warning("Failed to save results: {e$message}")
  })
}

#' Print Method for Single PheProb Results
#'
#' Custom print method for single PheProb results.
#'
#' @param x Single PheProb results object
#' @param ... Additional arguments (ignored)
#'
#' @export
print.pheprob_single <- function(x, ...) {
  
  cat("Single PheProb Results\n")
  cat("══════════════════════\n\n")
  
  # Basic information
  n_patients <- attr(x, "n_patients") %||% nrow(x)
  n_concepts <- attr(x, "n_concepts") %||% "unknown"
  method <- attr(x, "method") %||% "original_pheprob"
  
  cat("Analysis Summary:\n")
  cat("  Method:", method, "\n")
  cat("  Patients:", n_patients, "\n")
  cat("  Concepts:", n_concepts, "\n")
  
  # Model information
  if (!is.null(attr(x, "convergence"))) {
    cat("  Convergence:", attr(x, "convergence"), "\n")
    cat("  Iterations:", attr(x, "n_iterations") %||% "unknown", "\n")
  }
  
  # Probability summary
  if ("pheprob_score" %in% names(x)) {
    probs <- x$pheprob_score
    cat("\nPhenotype Probabilities:\n")
    cat("  Mean:", round(mean(probs, na.rm = TRUE), 3), "\n")
    cat("  Range:", round(min(probs, na.rm = TRUE), 3), "-", round(max(probs, na.rm = TRUE), 3), "\n")
    cat("  High confidence cases (>0.8):", sum(probs > 0.8, na.rm = TRUE), "\n")
    cat("  High confidence controls (<0.2):", sum(probs < 0.2, na.rm = TRUE), "\n")
  }
  
  cat("\nUse summary() for detailed model information\n")
  cat("Use plot() to visualize results\n")
}

#' Summary Method for Single PheProb Results
#'
#' Custom summary method for single PheProb results.
#'
#' @param object Single PheProb results object
#' @param ... Additional arguments (ignored)
#'
#' @export
summary.pheprob_single <- function(object, ...) {
  
  cat("Detailed Single PheProb Summary\n")
  cat("═══════════════════════════════\n\n")
  
  # Model parameters
  if (!is.null(attr(object, "parameters"))) {
    params <- attr(object, "parameters")
    cat("Model Parameters:\n")
    cat("  p_1 (case success rate):", round(params$p_1, 4), "\n")
    cat("  p_0 (control success rate):", round(params$p_0, 4), "\n")
    cat("  α_0 (intercept):", round(params$alpha_0, 4), "\n")
    cat("  α_1 (utilization effect):", round(params$alpha_1, 4), "\n")
    cat("  Parameter separation:", round(abs(params$p_1 - params$p_0), 4), "\n\n")
  }
  
  # Data summary
  if ("total_codes" %in% names(object) && "relevant_codes" %in% names(object)) {
    cat("Data Summary:\n")
    cat("  Total codes (C): mean =", round(mean(object$total_codes, na.rm = TRUE), 1),
        ", range =", min(object$total_codes, na.rm = TRUE), "-", max(object$total_codes, na.rm = TRUE), "\n")
    cat("  Relevant codes (S): mean =", round(mean(object$relevant_codes, na.rm = TRUE), 1),
        ", range =", min(object$relevant_codes, na.rm = TRUE), "-", max(object$relevant_codes, na.rm = TRUE), "\n")
    cat("  Success rate (S/C): mean =", round(mean(object$success_rate, na.rm = TRUE), 3), 
        ", sd =", round(sd(object$success_rate, na.rm = TRUE), 3), "\n\n")
  }
  
  # Model diagnostics
  if (!is.null(attr(object, "diagnostics"))) {
    diag <- attr(object, "diagnostics")
    cat("Model Diagnostics:\n")
    cat("  Log-likelihood:", round(attr(object, "log_likelihood"), 2), "\n")
    cat("  BIC:", round(diag$model_selection$BIC, 2), "\n")
    
    if (!is.null(diag$utilization_effect)) {
      cat("  Healthcare utilization effect (α_1):", round(diag$utilization_effect$alpha_1, 4), "\n")
    }
    cat("\n")
  }
  
  # Data quality
  if (!is.null(attr(object, "data_validation"))) {
    validation <- attr(object, "data_validation")
    cat("Data Quality:\n")
    cat("  Overall score:", round(validation$overall_quality$score, 1), "/100 (", validation$overall_quality$interpretation, ")\n")
    
    if (length(validation$warnings) > 0) {
      cat("  Warnings:\n")
      for (warning in validation$warnings) {
        cat("    •", warning, "\n")
      }
    }
  }
  
  cat("\nUse plot() to visualize probability distributions and model diagnostics\n")
}
