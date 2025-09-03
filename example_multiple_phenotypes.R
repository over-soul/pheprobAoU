# Example: Multiple Phenotypes with PheProb
# 
# This script demonstrates the multiple phenotypes extension using the
# PheProb binomial mixture model methodology.

# Load the package
library(pheprobAoU)

# =============================================================================
# EXAMPLE 1: Multiple Phenotypes - The Right Way
# =============================================================================

cat("=== MULTIPLE PHENOTYPES: PHEPROB METHOD (SINNOTT ET AL., 2018) ===\n\n")

# Define separate, clinically coherent phenotypes
research_phenotypes <- list(
  diabetes = c(201820, 201826, 4193704),           # Diabetes concepts
  cardiovascular = c(4329847, 313217, 442604, 316139),    # CVD concepts
  mental_health = c(4152280, 442077, 436665, 436676),   # Mental health concepts
  kidney_disease = c(4030518, 192359, 4030319, 4030320)   # CKD concepts
)

cat("Phenotype definitions:\n")
for (phenotype_name in names(research_phenotypes)) {
  concepts <- research_phenotypes[[phenotype_name]]
  cat("  •", phenotype_name, ":", length(concepts), "concepts\n")
}
cat("\n")

# Calculate multiple phenotype probabilities with concept hierarchy expansion
multi_results <- calculate_multiple_pheprobs(
  phenotype_concepts = research_phenotypes,
  phenotype_correlation_analysis = TRUE,
  joint_validation = TRUE,
  model_diagnostics = TRUE,
  progress = TRUE
  # Now uses the same improved extraction as single phenotypes!
  # - Concept hierarchy expansion with DISTINCT
  # - Direct SQL approach  
  # - Realistic healthcare utilization
)

# Display results
print(multi_results)
summary(multi_results)

# =============================================================================
# EXAMPLE 2: Understanding the Multiple Phenotypes Process
# =============================================================================

cat("\n=== HOW MULTIPLE PHENOTYPES WORKS ===\n\n")

cat("Process Overview:\n")
cat("1. Extract total healthcare utilization (C) ONCE for all patients\n")
cat("2. For each phenotype k:\n")
cat("   a. Extract disease-relevant codes (S_k)\n")
cat("   b. Create (S_k, C) pairs\n") 
cat("   c. Fit binomial mixture model: S_k ~ Binomial(C, p_y)\n")
cat("   d. Calculate P(Y=1|S_k,C) for phenotype k\n")
cat("3. Combine results across phenotypes\n")
cat("4. Analyze correlations and comorbidities\n\n")

# Simulate the process to show data structure
set.seed(42)
n_patients <- 500

# Step 1: Total healthcare utilization (shared)
C <- rpois(n_patients, lambda = 25) + 1

# Step 2: Phenotype-specific relevant codes
S_diabetes <- rbinom(n_patients, C, 0.15)      # 15% diabetes-relevant rate
S_cvd <- rbinom(n_patients, C, 0.12)           # 12% CVD-relevant rate  
S_mental <- rbinom(n_patients, C, 0.08)        # 8% mental health-relevant rate
S_kidney <- rbinom(n_patients, C, 0.05)        # 5% kidney disease-relevant rate

# Show example data structure
example_data <- tibble(
  person_id = 1:20,  # Show first 20 patients
  C = C[1:20],
  S_diabetes = S_diabetes[1:20],
  S_cvd = S_cvd[1:20], 
  S_mental = S_mental[1:20],
  S_kidney = S_kidney[1:20]
)

cat("Example patient data structure:\n")
print(example_data)

# =============================================================================
# EXAMPLE 3: Correlation and Comorbidity Analysis
# =============================================================================

cat("\n=== CORRELATION AND COMORBIDITY ANALYSIS ===\n\n")

# Extract correlation analysis from results
correlation_analysis <- attr(multi_results, "correlation_analysis")

if (!is.null(correlation_analysis)) {
  cat("Correlation Analysis Results:\n")
  cat("  Phenotype pairs tested:", correlation_analysis$summary$n_pairs_tested, "\n")
  cat("  Strong correlations (|r| > 0.3):", correlation_analysis$summary$n_strong_correlations, "\n")
  cat("  Maximum correlation:", round(correlation_analysis$summary$max_correlation, 3), "\n")
  cat("  Mean correlation:", round(correlation_analysis$summary$mean_correlation, 3), "\n\n")
  
  # Show correlation matrix
  cat("Correlation Matrix:\n")
  corr_matrix <- correlation_analysis$correlation_matrix
  print(round(corr_matrix, 3))
  
  # Show strongest correlations
  strong_corrs <- correlation_analysis$strong_correlations
  if (length(strong_corrs) > 0) {
    cat("\nStrongest correlations:\n")
    for (i in 1:min(3, length(strong_corrs))) {
      corr <- strong_corrs[[i]]
      cat("  •", corr$phenotype1, "~", corr$phenotype2, 
          ": r =", round(corr$correlation, 3),
          ", p =", format.pval(corr$p_value), "\n")
    }
  }
  
  # Comorbidity patterns
  comorb <- correlation_analysis$comorbidity_patterns
  if (!is.null(comorb)) {
    cat("\nComorbidity Analysis:\n")
    cat("  Patients with multiple high-prob phenotypes:", comorb$patients_with_multiple, "\n")
    cat("  Mean phenotypes per patient:", round(comorb$mean_phenotypes_per_patient, 2), "\n")
    
    if (length(comorb$most_common_pairs) > 0) {
      cat("  Most common comorbidity pairs:\n")
      for (i in 1:min(3, length(comorb$most_common_pairs))) {
        pair <- comorb$most_common_pairs[[i]]
        cat("    ", pair$phenotype1, "+", pair$phenotype2, 
            ":", pair$n_comorbid, "patients\n")
      }
    }
  }
}

# =============================================================================
# EXAMPLE 4: Model Diagnostics Across Phenotypes
# =============================================================================

cat("\n=== MODEL DIAGNOSTICS ===\n\n")

# Convergence summary
convergence_summary <- attr(multi_results, "convergence_summary")
if (!is.null(convergence_summary)) {
  cat("EM Algorithm Convergence:\n")
  cat("  Overall convergence rate:", round(convergence_summary$convergence_rate * 100, 1), "%\n")
  cat("  Mean iterations:", round(convergence_summary$mean_iterations, 1), "\n")
  
  # Individual convergence details
  individual_conv <- convergence_summary$individual_convergence
  cat("\nIndividual phenotype convergence:\n")
  for (phenotype_name in names(individual_conv)) {
    conv_info <- individual_conv[[phenotype_name]]
    status <- ifelse(conv_info$converged, "✓", "✗")
    cat("  ", status, phenotype_name, ": ", conv_info$iterations, " iterations\n")
  }
}

# Joint validation
joint_validation <- attr(multi_results, "joint_validation")
if (!is.null(joint_validation)) {
  cat("\nJoint Validation:\n")
  cat("  Mean data quality score:", round(joint_validation$mean_quality_score, 1), "/100\n")
  
  if (length(joint_validation$cross_phenotype_warnings) > 0) {
    cat("  Cross-phenotype warnings:\n")
    for (warning in joint_validation$cross_phenotype_warnings) {
      cat("    •", warning, "\n")
    }
  }
}

# =============================================================================
# EXAMPLE 5: Comparing with the Old Problematic Approach
# =============================================================================

cat("\n=== COMPARISON: OLD vs NEW APPROACH ===\n\n")

cat("❌ OLD APPROACH (Problematic):\n")
cat("mixed_concepts <- c(201820, 313217, 432870, 4030518)  # Mixing diabetes, CVD, mental health, kidney\n")
cat("mixed_scores <- calculate_pheprob(mixed_concepts)      # Single meaningless score\n")
cat("# Result: pheprob_score = 0.73  ← What does this mean??\n\n")

cat("✅ NEW APPROACH (Correct):\n")
cat("phenotypes <- list(\n")
cat("  diabetes = c(201820, 201826, 4193704),\n")
cat("  cardiovascular = c(4329847, 313217, 442604, 316139),\n")
cat("  mental_health = c(4152280, 442077, 436665, 436676),\n")
cat("  kidney_disease = c(4030518, 4030319)\n")
cat(")\n")
cat("multi_scores <- calculate_multiple_pheprobs(phenotypes)\n")
cat("# Result: Separate interpretable probabilities for each phenotype!\n\n")

# Show example output format
cat("Example output (wide format):\n")
example_output <- tibble(
  person_id = c(123456, 234567, 345678),
  diabetes_prob = c(0.89, 0.23, 0.67),
  cardiovascular_prob = c(0.45, 0.91, 0.12),
  mental_health_prob = c(0.12, 0.67, 0.89),
  kidney_disease_prob = c(0.23, 0.08, 0.34)
)
print(example_output)

# =============================================================================
# EXAMPLE 6: Visualization (if packages available)
# =============================================================================

cat("\n=== VISUALIZATION EXAMPLES ===\n\n")

cat("Available plot types:\n")
cat("  • plot(results, type='correlation_matrix')     - Phenotype correlations\n")
cat("  • plot(results, type='probability_distributions') - Probability histograms\n") 
cat("  • plot(results, type='comorbidity_network')    - Comorbidity patterns\n")
cat("  • plot(results, type='convergence_diagnostics') - Model convergence\n\n")

# Try to create plots (will gracefully handle missing packages)
tryCatch({
  cat("Creating correlation matrix plot...\n")
  plot(multi_results, type = "correlation_matrix")
}, error = function(e) {
  cat("Correlation plot failed (missing packages):", e$message, "\n")
})

tryCatch({
  cat("Creating probability distribution plots...\n")
  plot(multi_results, type = "probability_distributions")  
}, error = function(e) {
  cat("Distribution plot failed (missing packages):", e$message, "\n")
})

# =============================================================================
# EXAMPLE 7: Saving and Loading Results
# =============================================================================

cat("\n=== SAVING AND LOADING RESULTS ===\n\n")

cat("Saving results to different formats:\n")
cat("# Save as CSV (probabilities only)\n")
cat("calculate_multiple_pheprobs(..., output_file = 'results.csv')\n\n")

cat("# Save as RDS (full object with metadata)\n") 
cat("calculate_multiple_pheprobs(..., output_file = 'results.rds')\n\n")

cat("# Save as Excel with multiple sheets\n")
cat("calculate_multiple_pheprobs(..., output_file = 'results.xlsx')\n\n")

cat("Loading saved results:\n")
cat("results <- readRDS('results.rds')\n")
cat("summary(results)  # Full analysis preserved\n\n")

cat("=== MULTIPLE PHENOTYPES IMPLEMENTATION COMPLETE ===\n")
cat("✅ Individual binomial mixture model per phenotype\n")
cat("✅ Shared healthcare utilization adjustment\n") 
cat("✅ Correlation and comorbidity analysis\n")
cat("✅ Joint validation across phenotypes\n")
cat("✅ Rich visualization and diagnostics\n")
cat("✅ Multiple output formats\n")
cat("✅ Backward compatibility maintained\n\n")

cat("This approach provides:\n")
cat("• Separate interpretable probabilities for each phenotype\n")
cat("• Proper statistical modeling for each condition\n") 
cat("• Analysis of relationships between phenotypes\n")
cat("• Clinical research-ready results\n")
