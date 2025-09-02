#' Visualization Functions for Multiple Phenotypes
#'
#' This file contains visualization and display functions for multiple phenotype
#' analysis using the PheProb methodology (Sinnott et al., 2018).
#'
#' @name multiple_phenotypes_viz
NULL

#' Print Method for Multiple Original PheProb Results
#'
#' Custom print method for multiple original PheProb results.
#'
#' @param x Multiple original PheProb results object
#' @param ... Additional arguments (ignored)
#'
#' @export
print.pheprob_multiple <- function(x, ...) {
  
  cat("Multiple Phenotype PheProb Results (Original Method)\n")
  cat("═══════════════════════════════════════════════════\n\n")
  
  # Basic information
  n_patients <- attr(x, "n_patients") %||% nrow(x)
  n_phenotypes <- attr(x, "n_phenotypes") %||% "unknown"
  phenotype_names <- attr(x, "phenotype_names") %||% "unknown"
  method <- attr(x, "method") %||% "original_pheprob_multiple"
  
  cat("Analysis Summary:\n")
  cat("  Method:", method, "\n")
  cat("  Patients:", n_patients, "\n")
  cat("  Phenotypes:", n_phenotypes, "\n")
  
  if (length(phenotype_names) <= 5) {
    cat("  Phenotype names:", paste(phenotype_names, collapse = ", "), "\n")
  } else {
    cat("  Phenotype names:", paste(c(phenotype_names[1:3], "..."), collapse = ", "), "\n")
  }
  
  # Convergence information
  convergence_summary <- attr(x, "convergence_summary")
  if (!is.null(convergence_summary)) {
    cat("  Model convergence:", convergence_summary$converged_count, "/", convergence_summary$total_count, "\n")
  }
  
  # Probability summaries
  prob_cols <- names(x)[grepl("_prob$", names(x))]
  if (length(prob_cols) > 0) {
    cat("\nPhenotype Probability Summaries:\n")
    for (col in prob_cols[1:min(3, length(prob_cols))]) {
      phenotype_name <- sub("_prob$", "", col)
      probs <- x[[col]]
      mean_prob <- round(mean(probs, na.rm = TRUE), 3)
      high_conf <- sum(probs > 0.8, na.rm = TRUE)
      cat("  ", phenotype_name, ": mean =", mean_prob, ", high confidence =", high_conf, "\n")
    }
    
    if (length(prob_cols) > 3) {
      cat("  ... and", length(prob_cols) - 3, "more phenotypes\n")
    }
  }
  
  # Correlation summary
  correlation_analysis <- attr(x, "correlation_analysis")
  if (!is.null(correlation_analysis)) {
    n_strong <- length(correlation_analysis$strong_correlations)
    max_corr <- round(correlation_analysis$summary$max_correlation, 3)
    cat("\nPhenotype Correlations:\n")
    cat("  Strong correlations (|r| > 0.3):", n_strong, "\n")
    cat("  Maximum correlation:", max_corr, "\n")
  }
  
  cat("\nUse summary() for detailed analysis\n")
  cat("Use plot() to visualize results\n")
}

#' Summary Method for Multiple Original PheProb Results
#'
#' Custom summary method for multiple original PheProb results.
#'
#' @param object Multiple original PheProb results object
#' @param ... Additional arguments (ignored)
#'
#' @export
summary.pheprob_multiple <- function(object, ...) {
  
  cat("Detailed Multiple Phenotype PheProb Summary\n")
  cat("══════════════════════════════════════════════\n\n")
  
  # Analysis overview
  phenotype_names <- attr(object, "phenotype_names") %||% c()
  n_phenotypes <- length(phenotype_names)
  
  cat("Analysis Overview:\n")
  cat("  Phenotypes analyzed:", n_phenotypes, "\n")
  cat("  Patients included:", nrow(object), "\n")
  cat("  Processing time:", format(attr(object, "calculation_time")), "\n\n")
  
  # Individual phenotype summaries
  cat("Individual Phenotype Results:\n")
  prob_cols <- paste0(phenotype_names, "_prob")
  relevant_cols <- paste0(phenotype_names, "_relevant_codes")
  
  for (i in seq_along(phenotype_names)) {
    phenotype_name <- phenotype_names[i]
    prob_col <- prob_cols[i]
    rel_col <- relevant_cols[i]
    
    if (prob_col %in% names(object)) {
      probs <- object[[prob_col]]
      rel_codes <- if (rel_col %in% names(object)) object[[rel_col]] else rep(NA, length(probs))
      
      cat("  ", phenotype_name, ":\n")
      cat("    Probability: mean =", round(mean(probs, na.rm = TRUE), 3),
          ", sd =", round(sd(probs, na.rm = TRUE), 3), "\n")
      cat("    Range:", round(min(probs, na.rm = TRUE), 3), "-", round(max(probs, na.rm = TRUE), 3), "\n")
      cat("    High confidence (>0.8):", sum(probs > 0.8, na.rm = TRUE), 
          " (", round(100 * mean(probs > 0.8, na.rm = TRUE), 1), "%)\n")
      
      if (!all(is.na(rel_codes))) {
        cat("    Relevant codes: mean =", round(mean(rel_codes, na.rm = TRUE), 1),
            ", max =", max(rel_codes, na.rm = TRUE), "\n")
      }
      cat("\n")
    }
  }
  
  # Model convergence details
  convergence_summary <- attr(object, "convergence_summary")
  if (!is.null(convergence_summary)) {
    cat("Model Convergence:\n")
    cat("  Overall rate:", round(convergence_summary$convergence_rate * 100, 1), "%\n")
    cat("  Mean iterations:", round(convergence_summary$mean_iterations, 1), "\n")
    
    individual_conv <- convergence_summary$individual_convergence
    failed_phenotypes <- names(individual_conv)[!sapply(individual_conv, function(x) x$converged)]
    
    if (length(failed_phenotypes) > 0) {
      cat("  Failed to converge:", paste(failed_phenotypes, collapse = ", "), "\n")
    }
    cat("\n")
  }
  
  # Correlation analysis
  correlation_analysis <- attr(object, "correlation_analysis")
  if (!is.null(correlation_analysis)) {
    cat("Phenotype Correlation Analysis:\n")
    cat("  Phenotype pairs tested:", correlation_analysis$summary$n_pairs_tested, "\n")
    cat("  Strong correlations (|r| > 0.3):", correlation_analysis$summary$n_strong_correlations, "\n")
    cat("  Mean absolute correlation:", round(correlation_analysis$summary$mean_correlation, 3), "\n")
    cat("  Maximum correlation:", round(correlation_analysis$summary$max_correlation, 3), "\n")
    
    # Show strongest correlations
    strong_corrs <- correlation_analysis$strong_correlations
    if (length(strong_corrs) > 0) {
      cat("\n  Strongest correlations:\n")
      for (i in 1:min(3, length(strong_corrs))) {
        corr_data <- strong_corrs[[i]]
        cat("    ", corr_data$phenotype1, " ~ ", corr_data$phenotype2, 
            ": r =", round(corr_data$correlation, 3),
            " (p =", format.pval(corr_data$p_value), ")\n")
      }
    }
    cat("\n")
  }
  
  # Comorbidity patterns
  if (!is.null(correlation_analysis$comorbidity_patterns)) {
    comorb <- correlation_analysis$comorbidity_patterns
    cat("Comorbidity Analysis:\n")
    cat("  Patients with multiple phenotypes:", comorb$patients_with_multiple, "\n")
    cat("  Mean phenotypes per patient:", round(comorb$mean_phenotypes_per_patient, 2), "\n")
    
    if (length(comorb$most_common_pairs) > 0) {
      cat("  Most common comorbidity pairs:\n")
      for (i in 1:min(3, length(comorb$most_common_pairs))) {
        pair <- comorb$most_common_pairs[[i]]
        cat("    ", pair$phenotype1, " + ", pair$phenotype2, 
            ": ", pair$n_comorbid, " patients (", 
            round(pair$rate_comorbid * 100, 1), "%)\n")
      }
    }
    cat("\n")
  }
  
  # Data quality overview
  joint_validation <- attr(object, "joint_validation")
  if (!is.null(joint_validation)) {
    cat("Data Quality Overview:\n")
    cat("  Mean quality score:", round(joint_validation$mean_quality_score, 1), "/100\n")
    cat("  High quality phenotypes (≥80):", joint_validation$validation_summary$n_high_quality, "\n")
    
    if (length(joint_validation$cross_phenotype_warnings) > 0) {
      cat("  Cross-phenotype warnings:\n")
      for (warning in joint_validation$cross_phenotype_warnings) {
        cat("    •", warning, "\n")
      }
    }
  }
  
  cat("\nUse plot() to visualize correlation matrix and probability distributions\n")
}

#' Plot Method for Multiple Original PheProb Results
#'
#' Custom plot method for multiple original PheProb results.
#'
#' @param x Multiple original PheProb results object
#' @param type Type of plot: "correlation_matrix", "probability_distributions", 
#'   "comorbidity_network", "convergence_diagnostics"
#' @param ... Additional arguments passed to plotting functions
#'
#' @export
plot.pheprob_multiple <- function(x, type = "correlation_matrix", ...) {
  
  switch(type,
    "correlation_matrix" = plot_correlation_matrix(x, ...),
    "probability_distributions" = plot_probability_distributions(x, ...),
    "comorbidity_network" = plot_comorbidity_network(x, ...),
    "convergence_diagnostics" = plot_convergence_diagnostics(x, ...),
    cli::cli_abort("Unknown plot type: {type}. Available types: correlation_matrix, probability_distributions, comorbidity_network, convergence_diagnostics")
  )
}

#' Plot Correlation Matrix
#'
#' Creates a correlation matrix plot for multiple phenotypes.
#'
#' @param results Multiple phenotype results object
#' @param ... Additional arguments
#'
#' @keywords internal
plot_correlation_matrix <- function(results, ...) {
  
  correlation_analysis <- attr(results, "correlation_analysis")
  if (is.null(correlation_analysis)) {
    cli::cli_abort("No correlation analysis found. Set phenotype_correlation_analysis = TRUE.")
  }
  
  correlation_matrix <- correlation_analysis$correlation_matrix
  phenotype_names <- attr(results, "phenotype_names")
  
  # Check if required packages are available
  if (!requireNamespace("ggplot2", quietly = TRUE) || !requireNamespace("reshape2", quietly = TRUE)) {
    # Fallback to base R plot
    plot_correlation_matrix_base(correlation_matrix, phenotype_names, ...)
    return(invisible())
  }
  
  # ggplot2 version
  # Convert correlation matrix to long format
  corr_long <- reshape2::melt(correlation_matrix)
  names(corr_long) <- c("Phenotype1", "Phenotype2", "Correlation")
  
  # Create plot
  p <- ggplot2::ggplot(corr_long, ggplot2::aes(x = .data$Phenotype1, y = .data$Phenotype2, fill = .data$Correlation)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                                 limit = c(-1, 1), space = "Lab", name = "Correlation") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::coord_fixed() +
    ggplot2::labs(title = "Phenotype Correlation Matrix",
                 subtitle = "Correlations between phenotype probabilities",
                 x = "Phenotype", y = "Phenotype") +
    ggplot2::geom_text(ggplot2::aes(label = round(.data$Correlation, 2)), color = "black", size = 3)
  
  print(p)
}

#' Plot Correlation Matrix (Base R)
#'
#' Base R fallback for correlation matrix plotting.
#'
#' @param correlation_matrix Correlation matrix
#' @param phenotype_names Phenotype names
#' @param ... Additional arguments
#'
#' @keywords internal
plot_correlation_matrix_base <- function(correlation_matrix, phenotype_names, ...) {
  
  # Check for null or empty matrix
  if (is.null(correlation_matrix) || nrow(correlation_matrix) == 0 || ncol(correlation_matrix) == 0) {
    plot(1, 1, type = "n", main = "No correlation data available", 
         xlab = "", ylab = "", axes = FALSE)
    return(invisible())
  }
  
  # Create color palette
  n_colors <- 100
  colors <- colorRampPalette(c("blue", "white", "red"))(n_colors)
  
  # Map correlations to colors
  corr_range <- range(correlation_matrix, na.rm = TRUE)
  corr_scaled <- (correlation_matrix - corr_range[1]) / (corr_range[2] - corr_range[1])
  color_indices <- round(corr_scaled * (n_colors - 1)) + 1
  
  # Create plot
  image(1:nrow(correlation_matrix), 1:ncol(correlation_matrix), correlation_matrix,
        col = colors, axes = FALSE, xlab = "Phenotype", ylab = "Phenotype",
        main = "Phenotype Correlation Matrix")
  
  # Add axis labels
  axis(1, at = 1:nrow(correlation_matrix), labels = rownames(correlation_matrix), las = 2)
  axis(2, at = 1:ncol(correlation_matrix), labels = colnames(correlation_matrix), las = 1)
  
  # Add correlation values as text
  for (i in 1:nrow(correlation_matrix)) {
    for (j in 1:ncol(correlation_matrix)) {
      if (!is.na(correlation_matrix[i, j])) {
        text(i, j, round(correlation_matrix[i, j], 2), col = "black", cex = 0.8)
      }
    }
  }
  
  # Add color legend
  legend("right", legend = c("1", "0", "-1"), fill = c("red", "white", "blue"),
         title = "Correlation", cex = 0.8)
}

#' Plot Probability Distributions
#'
#' Creates probability distribution plots for multiple phenotypes.
#'
#' @param results Multiple phenotype results object
#' @param ... Additional arguments
#'
#' @keywords internal
plot_probability_distributions <- function(results, ...) {
  
  phenotype_names <- attr(results, "phenotype_names")
  prob_cols <- paste0(phenotype_names, "_prob")
  prob_data <- results[prob_cols]
  names(prob_data) <- phenotype_names
  
  # Check if ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE) || !requireNamespace("tidyr", quietly = TRUE)) {
    # Fallback to base R
    plot_probability_distributions_base(prob_data, ...)
    return(invisible())
  }
  
  # ggplot2 version
  prob_long <- prob_data %>%
    dplyr::mutate(patient_id = dplyr::row_number()) %>%
    tidyr::pivot_longer(cols = -"patient_id", names_to = "Phenotype", values_to = "Probability")
  
  p <- ggplot2::ggplot(prob_long, ggplot2::aes(x = .data$Probability, fill = .data$Phenotype)) +
    ggplot2::geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
    ggplot2::facet_wrap(~ .data$Phenotype, scales = "free_y") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Phenotype Probability Distributions",
                 subtitle = "Distribution of probabilities across patients",
                 x = "Phenotype Probability", y = "Count") +
    ggplot2::theme(legend.position = "none")
  
  print(p)
}

#' Plot Probability Distributions (Base R)
#'
#' Base R fallback for probability distribution plotting.
#'
#' @param prob_data Probability data
#' @param ... Additional arguments
#'
#' @keywords internal
plot_probability_distributions_base <- function(prob_data, ...) {
  
  # Check for null or empty data
  if (is.null(prob_data) || ncol(prob_data) == 0 || nrow(prob_data) == 0) {
    plot(1, 1, type = "n", main = "No probability data available", 
         xlab = "", ylab = "", axes = FALSE)
    return(invisible())
  }
  
  n_phenotypes <- ncol(prob_data)
  phenotype_names <- names(prob_data)
  
  # Set up multi-panel plot
  par(mfrow = c(ceiling(sqrt(n_phenotypes)), ceiling(sqrt(n_phenotypes))))
  
  # Create histogram for each phenotype
  for (i in 1:n_phenotypes) {
    hist(prob_data[[i]], main = paste("Distribution:", phenotype_names[i]),
         xlab = "Probability", ylab = "Frequency", col = "lightblue", border = "black")
  }
  
  # Reset par
  par(mfrow = c(1, 1))
}

#' Plot Comorbidity Network
#'
#' Creates a network plot showing comorbidity relationships.
#'
#' @param results Multiple phenotype results object
#' @param threshold Probability threshold for defining "positive" cases
#' @param ... Additional arguments
#'
#' @keywords internal
plot_comorbidity_network <- function(results, threshold = 0.8, ...) {
  
  correlation_analysis <- attr(results, "correlation_analysis")
  if (is.null(correlation_analysis) || is.null(correlation_analysis$comorbidity_patterns)) {
    cli::cli_abort("No comorbidity analysis found. Set phenotype_correlation_analysis = TRUE.")
  }
  
  # Simple comorbidity plot using base R
  comorbidity_pairs <- correlation_analysis$comorbidity_patterns$all_pairs
  
  if (length(comorbidity_pairs) == 0) {
    plot(1, 1, type = "n", main = "No significant comorbidity pairs found",
         xlab = "", ylab = "", axes = FALSE)
    return(invisible())
  }
  
  # Extract top pairs for visualization
  top_pairs <- head(comorbidity_pairs, 10)
  pair_names <- sapply(top_pairs, function(x) paste(x$phenotype1, "-", x$phenotype2))
  pair_counts <- sapply(top_pairs, function(x) x$n_comorbid)
  
  # Create bar plot
  barplot(pair_counts, names.arg = pair_names, las = 2, 
          main = "Most Common Comorbidity Pairs",
          ylab = "Number of Patients", col = "lightcoral")
}

#' Plot Convergence Diagnostics
#'
#' Creates diagnostic plots for model convergence.
#'
#' @param results Multiple phenotype results object
#' @param ... Additional arguments
#'
#' @keywords internal
plot_convergence_diagnostics <- function(results, ...) {
  
  convergence_summary <- attr(results, "convergence_summary")
  if (is.null(convergence_summary)) {
    cli::cli_abort("No convergence information found. Set model_diagnostics = TRUE.")
  }
  
  phenotype_names <- attr(results, "phenotype_names")
  individual_conv <- convergence_summary$individual_convergence
  
  # Check for null data
  if (is.null(phenotype_names) || is.null(individual_conv) || length(individual_conv) == 0) {
    plot(1, 1, type = "n", main = "No convergence data available", 
         xlab = "", ylab = "", axes = FALSE)
    return(invisible())
  }
  
  # Extract convergence information
  converged <- sapply(individual_conv, function(x) if (is.null(x)) FALSE else x$converged)
  iterations <- sapply(individual_conv, function(x) if (is.null(x)) NA else x$iterations)
  log_likelihood <- sapply(individual_conv, function(x) if (is.null(x)) NA else x$log_likelihood)
  
  # Set up multi-panel plot
  par(mfrow = c(2, 2))
  
  # 1. Convergence status
  convergence_colors <- ifelse(converged, "green", "red")
  barplot(as.numeric(converged), names.arg = phenotype_names, las = 2,
          col = convergence_colors, main = "Model Convergence Status",
          ylab = "Converged (1) / Failed (0)")
  
  # 2. Number of iterations
  barplot(iterations, names.arg = phenotype_names, las = 2,
          col = "lightblue", main = "EM Algorithm Iterations",
          ylab = "Number of Iterations")
  
  # 3. Log-likelihood values
  barplot(log_likelihood, names.arg = phenotype_names, las = 2,
          col = "lightgreen", main = "Final Log-Likelihood",
          ylab = "Log-Likelihood")
  
  # 4. Summary text
  plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Convergence Summary")
  text(1, 1, paste0("Convergence Rate: ", round(convergence_summary$convergence_rate * 100, 1), "%\n",
                   "Mean Iterations: ", round(convergence_summary$mean_iterations, 1), "\n",
                   "Total Models: ", convergence_summary$total_count),
       cex = 1.2, adj = 0.5)
  
  # Reset par
  par(mfrow = c(1, 1))
}
